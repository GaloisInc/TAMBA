#!/usr/bin/python
import os
import re
import sys
import copy
import time
from subprocess32 import check_output
from decimal import *

normal_pattern = re.compile("Revised max-belief: ([0-9]+)/([0-9]+)")
conc_pattern = re.compile("max-belief \(after improve lower bounds\): ([0-9]+)/([0-9]+)")
sampling_pattern = re.compile("max-belief \(post-sampling\): ([0-9]+)/([0-9]+)")

complexity_limit = int(sys.argv[1])
precision_limit = int(sys.argv[2])
sample_limit = int(sys.argv[3])
sample_increment = int(sys.argv[4])
timeout = int(sys.argv[5])
program_prefix = sys.argv[6]

print "analysis,complexity,precision,samples,time,max_belief"

os.chdir("/vagrant/code/prob")

for a in ["AI", "Box", "Concolic", "Sampling", "Both"]:
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
                    result = check_output(args_final, timeout=timeout)
                    after  = time.time()

                    elapsed = after - before

                    lex_result = normal_pattern.search(result)

                    if lex_result is None:
                        result = Decimal(1)
                    else:
                        num = Decimal(lex_result.group(1))
                        den = Decimal(lex_result.group(2))
                        result = num / den
                except:
                    result = Decimal(-1)
                    elapsed = float(timeout)

                for s in range(1, sample_limit + 1):
                    samples = s * sample_increment
                    print "%s,%d,%d,%d,%.50f,%.50f" % (a, c, p_scaled, samples, elapsed, result)
            else:
                for s in range(1, sample_limit + 1):
                    samples = s * sample_increment

                    args_final = copy.deepcopy(args)

                    if a == "Concolic":
                        args_final += ["--improve-lower-bounds", str(samples)]
                        pattern = conc_pattern
                    elif a == "Sampling":
                        args_final += ["--samples", str(samples)]
                        pattern = sampling_pattern
                    else:
                        args_final += ["--improve-lower-bounds", str(samples),
                                     "--samples", str(samples)]
                        pattern = sampling_pattern

                    args_final += [program_prefix + "-" + str(c) + ".prob"]

                    try:
                        before = time.time()
                        result = check_output(args_final, timeout=timeout)
                        after  = time.time()

                        elapsed = after - before

                        lex_result = pattern.search(result)

                        if lex_result is None:
                            result = Decimal(1)
                        else:
                            num = Decimal(lex_result.group(1))
                            den = Decimal(lex_result.group(2))
                            result = num / den
                    except:
                        result = Decimal(-1)
                        elapsed = float(timeout)

                    print "%s,%d,%d,%d,%.50f,%.50f" % (a, c, p_scaled, samples, elapsed, result)
