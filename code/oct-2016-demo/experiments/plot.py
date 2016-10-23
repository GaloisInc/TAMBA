#!/usr/local/bin/python

import sys
import pandas
import matplotlib.pyplot as plt

from decimal import *

data = pandas.read_csv(sys.argv[1])

num_samples = data.num_samples.tolist()
mb_lb = data.mb_lb.tolist()
mb_sampling = data.mb_sampling.tolist()
mb_lb_sampling = data.mb_lb_sampling.tolist()

plt.plot(num_samples, mb_lb, 'b', label='lower bound')
plt.plot(num_samples, mb_lb_sampling, 'r', label='lower bound + sampling')
plt.plot(num_samples, mb_sampling, 'g', label='sampling')

print num_samples
print mb_lb
print mb_sampling
print mb_lb_sampling

plt.xlabel('# samples')
plt.ylabel('max-belief')
plt.legend(loc=5)
plt.show()
