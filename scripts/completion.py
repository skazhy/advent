#!/usr/bin/env python3

# Script to generate links to solved puzzles.

import re
import subprocess

from collections import defaultdict

languages = {
    "advent": "Clojure",
    "haskell": "Haskell",
    "rust": "Rust"
}

def all_puzzles():
    puzzles = defaultdict(lambda: defaultdict(lambda: defaultdict(dict)))
    puzzle_re = r'src/([a-z]+)/[year]*(\d+)/(?:d|D)ay(\d+)'
    git_cmd = [
        "git", "ls-tree",
        "--full-tree", "--name-only", "-r", "HEAD",
        "--", "src"
    ]
    for file in subprocess.run(git_cmd, capture_output=True, text=True).stdout.split('\n'):
        if m := re.search(puzzle_re, file):
            puzzles[m.group(2)][m.group(3)][m.group(1)] = file
    return puzzles

def gen_markdown():
    puzzles = all_puzzles()
    with open("doc/PUZZLES.md", "w", encoding="utf-8") as doc:
        doc.write('# Solved Advent of Code puzzles\n\n')

        for year in sorted(puzzles.keys(), reverse=True):
            doc.write((f'## {year}\n\n'
                       '| Day | Solutions |\n'
                       '| --- | --------- |\n'
                      ))

            for day in sorted(puzzles[year], key=int):
                solutions = []
                for lang in sorted(puzzles[year][day], key=lambda x: languages[x]):
                    solutions.append(f'[{languages[lang]}](../{puzzles[year][day][lang]})')
                doc.write(f'| {day} | {", ".join(solutions)} |\n')

if __name__ == "__main__":
    print("Regenerating completed puzzle doc...")
    gen_markdown()
