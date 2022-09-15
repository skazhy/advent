# Advent of Code 2018, day 1: Chronal Calibration

import itertools

from functools import reduce
from helpers import int_readlines

data = int_readlines(2018, 1)

def puzzle2():
    seen = set([0])
    for i in itertools.accumulate(itertools.cycle(data)):
        if i in seen:
            return i
        seen.add(i)

print(reduce((lambda x,y: x+y), data))
print(puzzle2())
