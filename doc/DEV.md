# Tooling notes

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
