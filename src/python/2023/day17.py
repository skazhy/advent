# Advent of Code 2023, day 17: Clumsy Crucible
# https://adventofcode.com/2023/day/17

import math
import heapq
from helpers import readlines

# Recycled Dijkstra solution from 2021.15


def mk_graph(str_grid):
    graph = {}
    for y, row in enumerate(str_grid):
        for x, r in enumerate(row):
            graph[(x, y)] = int(r)
    return graph


def neighbors(graph, coords, direction, max_straight, min_turn):
    x, y = coords
    dx, dy = direction
    choices = []

    if min_turn <= abs(dx):
        choices = [((x, y - 1), (0, -1)), ((x, y + 1), (0, 1))]

    if 0 < abs(dx) < max_straight:
        sign = int(math.copysign(1, dx))
        choices.append((((x + 1 * sign), y), (dx + 1 * sign, 0)))

    if min_turn <= abs(dy):
        choices = [((x - 1, y), (-1, 0)), ((x + 1, y), (1, 0))]

    if 0 < abs(dy) < max_straight:
        sign = int(math.copysign(1, dy))
        choices.append(((x, (y + 1 * sign)), (0, dy + 1 * sign)))

    for coords, direction in choices:
        if coords in graph:
            yield (coords, direction)


def find_path(str_grid, max_straight=3, min_turn=1):
    graph = mk_graph(str_grid)
    target = (len(str_grid[0]) - 1, len(str_grid) - 1)

    pq = [(0, (0, 0), (1, 0))]
    heapq.heapify(pq)

    seen = set()
    while pq:
        loss, coords, direction = heapq.heappop(pq)
        dx, dy = direction

        if (coords, direction) in seen:
            continue

        if coords == target and (abs(dx) >= min_turn or abs(dy) >= min_turn):
            return loss

        seen.add((coords, direction))

        for coords, direction in neighbors(
            graph, coords, direction, max_straight, min_turn
        ):
            if ((coords, direction)) not in seen:
                heapq.heappush(pq, (loss + graph[coords], coords, direction))


data = readlines(2023, 17)

print(find_path(data, max_straight=3, min_turn=1))
print(find_path(data, max_straight=10, min_turn=4))
