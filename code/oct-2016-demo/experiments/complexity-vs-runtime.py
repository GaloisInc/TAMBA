#!/usr/local/bin/python

import sys
import pandas
from matplotlib.ticker import LogFormatterExponent
import matplotlib.pyplot as plt

from decimal import *

data = pandas.read_csv(sys.argv[1])

complexity = data.complexity.tolist()
poly = data.poly.tolist()
precise_boxes = data.precise_boxes.tolist()
imprecise_boxes = data.imprecise_boxes.tolist()

plt.plot(complexity, poly, 'b', label='polyhedra')
plt.plot(complexity, precise_boxes, 'r', label='boxes (precise)')
plt.plot(complexity, imprecise_boxes, 'g', label='boxes (imprecise)')

print complexity
print poly
print precise_boxes
print imprecise_boxes

plt.xlabel('complexity')
plt.ylabel('runtime (seconds)')
plt.yscale('log')
plt.gca().yaxis.set_minor_formatter(LogFormatterExponent(labelOnlyBase=False))
plt.legend(loc=1)
plt.show()
