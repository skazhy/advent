# Tooling notes

## Editor support

Development and testing happens within Doom Emacs - `aoc.el` has helper functions that call `aoc.sh` under the hood.
There's very basic Vim support for jumping between source / test data in `aoc.vim` (although it's not really used anymore).

## Supported languages

### Clojure

Puzzles are tested via `clojure.test` - a separate test file with well known function name is crated when creating a new test file.

### Idris

Idris 2 can be installed [via Homebrew](https://idris2.readthedocs.io/en/latest/tutorial/starting.html#installing-from-a-package-manager). Each puzzle is compiled as a separate binary. Puzzles are expected to output
one solution per line.

### Haskell

Puzzles are self-contained files that are run interactively with `runghc`. [hlint](https://github.com/ndmitchell/hlint) is used for linting puzzle source code.

### Python

Same as Haskell - puzzles are self contained scripts.

### Rust

All puzzles are compiled into a single binary, puzzle's day and year is passed as argument when running it. Shared files (`mod.rs`, etc) [will be updated automatically](../scripts/env/rust.sh#L17) when adding a new puzzle.

### Scheme

Puzzles are implemented with [Chicken Scheme](https://wiki.call-cc.org) & run in the interpreted mode.

## Adding a new language support

`aoc.sh` expects these methods to exist when dealing with puzzle code:

- `setup` - called before any other command. This function declares `SRC_FILE`,
  along with any other env variables and creates required folders for new
  source and test files.
- `gen_src_file_content` - called when requested puzzle does not exist,
  creates a new source file for the puzzle & performs other file changes that
  are needed to add a new puzzle to the project
- `run_assert` - called when `aoc.sh` is started with the `test` argument -
  tests if puzzle results are valid. See "Testing" section below
- `lint` - called when `aoc.sh` is started with the `lint` argument - performs
  language-specific formatting checks (method implementation needs to specify
  which file(s) are linted)

These methods can be defined in a separate bash script, which is stored in
`scripts/env` ([Python](../scripts/env/python.sh) setup is a good reference implementation).
This file then is sourced when the language is selected as a command line
argument in [aoc.sh](https://github.com/skazhy/advent/blob/master/aoc.sh#L15).

All scripts have access to the following env vars:

- `YEAR`, `MONTH`, `DAY`
- `PUZZLE_URL` - URL to the selected AoC puzzle

### Templating

New puzzle boilerplates can be saved as "templates" (here's [Haskell
template](https://github.com/skazhy/advent/blob/master/scripts/templates/haskell.txt)).
`eval_template` method from `utils.sh` can be used to eval a template in given
path & save it in a file elsewhere, these templates can contain all env vars
that are visible in scripts.

### Testing

The quickest way to add testing that puzzle returns correct results for the
input, is to use `assert` function from `utils.sh`. It takes a multiline
string (with each result in it's own line) as an input & compares it to values
from the `results/solutions` file for the same year and day.
