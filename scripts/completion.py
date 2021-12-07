#!/usr/bin/env python3

# Script to generate links to solved puzzles.

import os
import re

from collections import defaultdict

languages = {
    "advent": "Clojure",
    "haskell": "Haskell"
}

def handle_lang(lang):
    puzzles = []
    for folder in os.walk(f'src/{lang}'):
        (path, subdirs, files) = folder
        puzzle_year_re = re.search(r"20[\d]{2}", path)
        if not subdirs and puzzle_year_re:
            year = puzzle_year_re.group(0)

            for file in files:
                if puzzle_day_re := re.search(r"[\d]{1,2}", file):
                    puzzles.append((year, puzzle_day_re.group(0), f'{path}/{file}'))
    return puzzles

def all_puzzles():
    puzzles = defaultdict(lambda: defaultdict(lambda: defaultdict(dict)))
    for lang in languages:
        for (year, day, path) in handle_lang(lang):
            puzzles[year][day][lang] = path
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
