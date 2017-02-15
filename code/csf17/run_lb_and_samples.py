#!/usr/bin/python
import os
import re
import sys
import copy
import time
from subprocess import check_output
from decimal import *

normal_pattern = re.compile("Revised max-belief: ([0-9]+)/([0-9]+)")
lb_pattern = re.compile("max-belief \(after improve lower bounds\): ([0-9]+)/([0-9]+)")
sample_pattern = re.compile("max-belief \(post-sampling\): ([0-9]+)/([0-9]+)")

complexity_limit = int(sys.argv[1])
precision_limit = int(sys.argv[2])
sample_limit = int(sys.argv[3])
sample_increment = int(sys.argv[4])
program_prefix = sys.argv[5]

print "analysis,complexity,precision,samples,time,max_belief"

os.chdir("../prob")

for a in ["Box", "Concolic", "Sampling", "Both"]:
    for c in range(1, complexity_limit + 1):
        for p in range(0, precision_limit + 1):
            p_scaled = 2**p

            args = ["./prob", "--inline"]

            # Set analysis type
            if a == "AI":
                args += ["--domain", "poly"]
            else:
                args += ["--domain", "box"]

            # Set the precision
            args += ["--precision", str(p_scaled)]

            # If its AI or Box, just pre-compute the result and report same result for all samples
            if a == "AI" or a == "Box":
                args_final = copy.deepcopy(args)

                args_final += [program_prefix + "-" + str(c) + ".prob"]

                try:
                    before = time.time()
                    result = check_output(args_final)
                    after  = time.time()

                    elapsed = (after - before) * 1000

                    lex_result = normal_pattern.search(result)

                    if lex_result is None:
                        result = Decimal(1)
                    else:
                        num = Decimal(lex_result.group(1))
                        den = Decimal(lex_result.group(2))
                        result = num / den
                except:
                    result = Decimal(-1)
                    elapsed = float(60000)

                for s in range(1, sample_limit + 1):
                    samples = s * sample_increment
                    print "%s,%d,%d,%d,%.50f,%.50f" % (a, c, p_scaled, samples, elapsed, result)
            else:
                for s in range(1, sample_limit + 1):
                    samples = s * sample_increment

                    args_final = copy.deepcopy(args)

                    if a == "Concolic":
                        args_final += ["--improve-lower-bounds", str(samples)]
                    elif a == "Sampling":
                        args_final += ["--samples", str(samples)]
                    else:
                        args_final += ["--improve-lower-bounds", str(samples),
                                     "--samples", str(samples)]

                    args_final += [program_prefix + "-" + str(c) + ".prob"]

                    #result = check_output(args_final)
                    result = 50

                    print "%s,%d,%d,%d,%d,%.50f" % (a, c, p_scaled, samples, 1000, float(result))
