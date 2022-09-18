#!/usr/bin/env python3

# Script to generate links to solved puzzles.

import re
import subprocess

from collections import defaultdict

languages = {
    "advent": "Clojure",
    "haskell": "Haskell",
    "python": "Python",
    "rust": "Rust",
}

PUZZLE_RE = r"src/([a-z]+)/[year]*(\d+)/(?:d|D)ay(\d+)"


def find_puzzles(acc, git_cmd):
    cmd = ["git"] + git_cmd + ["--", "src"]
    for file in subprocess.run(cmd, capture_output=True, text=True).stdout.split(
        "\n"
    ):
        if m := re.search(PUZZLE_RE, file):
            acc[m.group(2)][m.group(3)][m.group(1)] = file
    return acc


def all_puzzles():
    puzzles = defaultdict(lambda: defaultdict(lambda: defaultdict(dict)))
    puzzles = find_puzzles(
        puzzles,
        [
            "ls-tree",
            "--full-tree",
            "--name-only",
            "-r",
            "HEAD",
        ],
    )
    return find_puzzles(puzzles, ["diff", "--name-only", "--cached"])


def gen_markdown():
    puzzles = all_puzzles()
    first_year = True
    with open("doc/PUZZLES.md", "w", encoding="utf-8") as doc:
        doc.write("# Solved Advent of Code puzzles\n\n")

        for year in sorted(puzzles.keys(), reverse=True):
            if not first_year:
                doc.write("\n")
            doc.write(
                (f"## {year}\n\n" "| Day | Solutions |\n" "| --- | --------- |\n")
            )

            for day in sorted(puzzles[year], key=int):
                solutions = []
                for lang in sorted(puzzles[year][day], key=lambda x: languages[x]):
                    solutions.append(
                        f"[{languages[lang]}](../{puzzles[year][day][lang]})"
                    )
                doc.write(f'| {day} | {", ".join(solutions)} |\n')
            first_year = False


if __name__ == "__main__":
    print("Regenerating completed puzzle doc...")
    gen_markdown()
