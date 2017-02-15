#!/usr/bin/python
import os
import re
import sys
from subprocess import check_output
from decimal import *

n = int(sys.argv[1])
size = int(sys.argv[2])

lb_pattern = re.compile("max-belief \(after improve lower bounds\): ([0-9]+)/([0-9]+)")
sample_pattern = re.compile("max-belief \(post-sampling\): ([0-9]+)/([0-9]+)")

print "num_samples,mb_lb,mb_sampling,mb_lb_sampling"

os.chdir("../../prob")

# lower bound
out_lb = check_output(["./prob", "--domain", "box", "--inline", "--improve-lower-bounds", "10000", "/vagrant/code/manhattan-pairwise-single-new.prob"])
lex_lb = lb_pattern.search(out_lb)
lb_num = Decimal(lex_lb.group(1))
lb_den = Decimal(lex_lb.group(2))
lb_val = lb_num / lb_den

for i in range(1, n + 1):
    num_samples = i * size

    out_samples = check_output(["./prob", "--domain", "box", "--inline", "--samples", str(num_samples), "/vagrant/code/manhattan-pairwise-single-new.prob"])
    lex_samples = sample_pattern.search(out_samples)
    samples_num = Decimal(lex_samples.group(1))
    samples_den = Decimal(lex_samples.group(2))
    samples_val = samples_num / samples_den

    out_lb_samples = check_output(["./prob", "--domain", "box", "--inline", "--improve-lower-bounds", "10000", "--samples", str(num_samples), "/vagrant/code/manhattan-pairwise-single-new.prob"])
    lex_lb_samples = sample_pattern.search(out_lb_samples)
    lb_samples_num = Decimal(lex_lb_samples.group(1))
    lb_samples_den = Decimal(lex_lb_samples.group(2))
    lb_samples_val = lb_samples_num / lb_samples_den

    print "%d,%.50f,%.50f,%.50f" % (num_samples, lb_val, samples_val, lb_samples_val)

    

