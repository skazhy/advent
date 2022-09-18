def input_path(year, day):
    return f"resources/{year}/day{day}.txt"


def readlines(year, day):
    return open(input_path(year, day), "r").readlines()


def int_readlines(year, day):
    return [int(r) for r in readlines(year, day)]
