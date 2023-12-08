# Advent of Code 2020, day 23: Crab Cups
# https://adventofcode.com/2020/day/23

from helpers import read

data = [int(x) for x in read(2020, 23)]


def cup_map(cups, p2=False):
    cm = {cups[-1]: cups[0]}
    for i in range(len(cups) - 1):
        cm[cups[i]] = cups[i + 1]

    if p2:
        cm[1000000] = cups[0]
        cm[cups[-1]] = 10
        for x in range(10, 1000000):
            cm[x] = x + 1
    return cm


def play_cups(cups, runs, p2=False):
    cm = cup_map(cups, p2=p2)
    head = cups[0]

    for _ in range(runs):
        taken = [cm[head]]
        taken += [cm[taken[0]]]
        taken += [cm[taken[1]]]

        dest = head - 1
        while True:
            if dest not in taken and dest in cm:
                break
            dest -= 1
            if dest not in cm:
                if p2:
                    dest = 1000000
                else:
                    dest = 9

        old_dest = cm[dest]
        cm[head] = cm[taken[-1]]
        cm[dest] = taken[0]
        cm[taken[-1]] = old_dest

        head = cm[head]
    return cm


def p1(input):
    cm = play_cups(input, 100, p2=False)
    s = ""
    x = 1
    for i in range(8):
        s += str(cm[x])
        x = cm[x]
    print(s)


def p2(input):
    cm = play_cups(input, 10000000, p2=True)
    print(cm[1] * cm[cm[1]])


p1(data)
p2(data)
