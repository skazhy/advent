# Advent of Code 2015, day 6: Probably a Fire Hazard
# https://adventofcode.com/2015/day/6

import re

from helpers import readlines
from collections import defaultdict

row_re = r"(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)"
data = [re.search(row_re, r) for r in readlines(2015, 6)]

# Part 1

grid1 = defaultdict(lambda: defaultdict(lambda: False))

for m in data:
    for x in range(int(m.group(2)), int(m.group(4)) + 1):
        for y in range(int(m.group(3)), int(m.group(5)) + 1):
            if m.group(1) == "toggle":
                grid1[x][y] = not grid1[x][y]
            else:
                grid1[x][y] = m.group(1) == "turn on"

res = 0
for row in grid1.values():
    for k in row.values():
        if k:
            res += 1
print(res)

# Part 2

grid2 = defaultdict(lambda: defaultdict(lambda: 0))

for m in data:
    for x in range(int(m.group(2)), int(m.group(4)) + 1):
        for y in range(int(m.group(3)), int(m.group(5)) + 1):
            if m.group(1) == "toggle":
                grid2[x][y] += 2
            elif m.group(1) == "turn off":
                grid2[x][y] = max(0, grid2[x][y] - 1)
            else:
                grid2[x][y] += 1

res = 0
for row in grid2.values():
    res += sum(row.values())

print(res)
