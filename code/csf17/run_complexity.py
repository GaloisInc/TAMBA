#!/usr/bin/python
import os
import re
import sys
from subprocess import check_output
from decimal import *

lb_pattern = re.compile("max-belief \(after improve lower bounds\): ([0-9]+)/([0-9]+)")
sample_pattern = re.compile("max-belief \(post-sampling\): ([0-9]+)/([0-9]+)")

def run_lb(samp, prog):
    out_lb = check_output(["./prob",
                           "--domain", "box",
                           "--inline",
                           "--improve-lower-bounds", str(samp),
                           program])

    lex_lb = lb_pattern.search(out_lb)

    if lex_lb is None:
        return Decimal(1)

    lb_num = Decimal(lex_lb.group(1))
    lb_den = Decimal(lex_lb.group(2))
    return lb_num / lb_den

def run_samp(samp, prog):
    out_samp = check_output(["./prob",
                             "--domain", "box",
                             "--inline",
                             "--samples", str(samp),
                             program])

    lex_samp = sample_pattern.search(out_samp)

    if lex_samp is None:
        return Decimal(1)

    samp_num = Decimal(lex_samp.group(1))
    samp_den = Decimal(lex_samp.group(2))
    return samp_num / samp_den

def run_both(samp_lb, samp_samp, prog):
    out_both = check_output(["./prob",
                             "--domain", "box",
                             "--inline",
                             "--improve-lower-bounds", str(samp_lb),
                             "--samples", str(samp_samp),
                             program])

    lex_both = sample_pattern.search(out_both)

    if lex_both is None:
        return Decimal(1)

    both_num = Decimal(lex_both.group(1))
    both_den = Decimal(lex_both.group(2))
    return both_num / both_den

n = int(sys.argv[1])
size = int(sys.argv[2])
program = sys.argv[3]

print "num_samples,mb_lb,mb_sampling,mb_lb_sampling"

os.chdir("../prob")

# lower bound
lb_val = run_lb(n * size, program)

for i in range(1, n + 1):
    num_samples = i * size

    samp_val = run_samp(num_samples, program)
    both_val = run_both(n * size, num_samples, program)
   
    print "%d,%.50f,%.50f,%.50f" % (num_samples, lb_val, samp_val, both_val)

    

