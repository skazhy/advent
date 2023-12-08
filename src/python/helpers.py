def input_path(year, day):
    return f"resources/{year}/day{day}.txt"


def read(year, day):
    return open(input_path(year, day), "r").read().strip()


def readlines(year, day):
    return [r.strip() for r in open(input_path(year, day), "r").readlines()]


def int_readlines(year, day):
    return [int(r) for r in readlines(year, day)]
