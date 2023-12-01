import re
import subprocess

from collections import defaultdict

languages = {
    "advent": "Clojure",
    "haskell": "Haskell",
    "python": "Python",
    "idris": "Idris",
    "rust": "Rust",
}

PUZZLE_RE = r"src/([a-z]+)/[year]*(\d+)/(?:d|D)ay(\d+)"


def run_git_cmd(cmd):
    return [
        x
        for x in subprocess.run(
            ["git"] + cmd, capture_output=True, text=True
        ).stdout.split("\n")
        if x
    ]


def find_puzzles(acc, git_cmd):
    for file in run_git_cmd(git_cmd + ["--", "src"]):
        if m := re.search(PUZZLE_RE, file):
            acc[m.group(2)][m.group(3)][languages[m.group(1)]] = file
    return acc


STAGED_FILE_CMD = ["diff", "--name-only", "--cached"]


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
    return find_puzzles(puzzles, STAGED_FILE_CMD)


def staged_years():
    return set(
        find_puzzles(
            defaultdict(lambda: defaultdict(lambda: defaultdict(dict))), STAGED_FILE_CMD
        ).keys()
    )
