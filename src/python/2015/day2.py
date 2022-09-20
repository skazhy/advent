# Advent of Code 2015, day 2: I Was Told There Would Be No Math
# https://adventofcode.com/2015/day/2

from helpers import readlines
from itertools import combinations

data = [[int(x) for x in row.strip().split("x")] for row in readlines(2015, 2)]

res1 = 0
res2 = 0
for row in data:
    side1, side2, side3 = sorted((x * y for x, y in combinations(row, 2)))
    res1 += side1 + 2 * side1 + 2 * side2 + 2 * side3

    edge1, edge2, edge3 = sorted(row)
    res2 += 2 * edge1 + 2 * edge2 + edge1 * edge2 * edge3

print(res1)
print(res2)
