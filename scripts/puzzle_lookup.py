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
    for file in subprocess.run(cmd, capture_output=True, text=True).stdout.split("\n"):
        if m := re.search(PUZZLE_RE, file):
            acc[m.group(2)][m.group(3)][languages[m.group(1)]] = file
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
