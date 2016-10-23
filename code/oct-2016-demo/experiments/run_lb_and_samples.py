#!/usr/bin/python
import os
import re
from subprocess import check_output
from decimal import *

ret = []
lb_pattern = re.compile("max-belief \(after improve lower bounds\): ([0-9]+)/([0-9]+)")
sample_pattern = re.compile("max-belief \(post-sampling\): ([0-9]+)/([0-9]+)")

print "num_samples,mb_lb,mb_sampling,mb_lb_sampling"

os.chdir("../../prob")

# lower bound
out_lb = check_output(["./prob", "--domain", "box", "--inline", "--improve-lower-bounds", "100", "../oct-2016-demo/manhattan-pairwise-single.prob"])
lex_lb = lb_pattern.search(out_lb)
lb_num = Decimal(lex_lb.group(1))
lb_den = Decimal(lex_lb.group(2))
lb_val = lb_num / lb_den


for num_samples in range(1, 41):
    samp_num = num_samples * 250

    out_samples = check_output(["./prob", "--domain", "box", "--inline", "--samples", str(samp_num), "../oct-2016-demo/manhattan-pairwise-single.prob"])
    lex_samples = sample_pattern.search(out_samples)
    samples_num = Decimal(lex_samples.group(1))
    samples_den = Decimal(lex_samples.group(2))
    samples_val = samples_num / samples_den

    out_lb_samples = check_output(["./prob", "--domain", "box", "--inline", "--improve-lower-bounds", "100", "--samples", str(samp_num), "../oct-2016-demo/manhattan-pairwise-single.prob"])
    lex_lb_samples = sample_pattern.search(out_lb_samples)
    lb_samples_num = Decimal(lex_lb_samples.group(1))
    lb_samples_den = Decimal(lex_lb_samples.group(2))
    lb_samples_val = lb_samples_num / lb_samples_den

    print "%d,%.50f,%.50f,%.50f" % (samp_num, lb_val, samples_val, lb_samples_val)

    

