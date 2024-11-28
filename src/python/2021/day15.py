# Advent of Code 2021, day 15: Chiton
# https://adventofcode.com/2021/day/15

import heapq
from helpers import readlines

data = readlines(2021, 15)


def to_int_grid(str_grid):
    return [[int(x) for x in row] for row in str_grid]


def neighbors(grid, coords):
    x, y = coords
    for coords in [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]:
        if coords in grid:
            yield coords


def find_path(grid):
    graph = {}
    for y, row in enumerate(grid):
        for x, r in enumerate(row):
            graph[(x, y)] = r

    target = (len(grid[0]) - 1, len(grid) - 1)

    # Dijkstra's algorithm for finding the shortest path across the given int grid.

    pq = [(0, (0, 0))]
    heapq.heapify(pq)

    seen = set()
    while pq:
        r, coords = heapq.heappop(pq)

        if coords in seen:
            continue

        if coords == target:
            return r

        seen.add(coords)
        for n in neighbors(graph, coords):
            if n not in seen:
                heapq.heappush(pq, (r + graph[n], n))


# Part 2: growing the grid 4 times


def bump_row(row):
    return [(x + 1) % 10 or 1 for x in row]


def extend_row(row):
    r = row
    last = row
    for x in range(4):
        last = bump_row(last)
        r += last
    return r


def extend_rows(rows):
    r = [extend_row(row) for row in rows]
    last = r
    for x in range(4):
        last = [bump_row(row) for row in last]
        r += last
    return r


def puzzle1(str_grid):
    return find_path(to_int_grid(str_grid))


def puzzle2(str_grid):
    return find_path(extend_rows(to_int_grid(str_grid)))


print(puzzle1(data))
print(puzzle2(data))
