# Advent of Code

```
           *             ,
                       _/^\_
                      <     >
     *                 /.-.\         *
              *        `/&\`                   *
                      ,@.*;@,
                     /_o.I %_\    *
        *           (`'--:o(_@;
                   /`;--.,__ `')             *
                  ;@`o % O,*`'`&\
            *    (`'--)_@ ;o %'()\      *
                 /`;--._`''--._O'@;
                /&*,()~o`;-.,_ `""`)
     *          /`,@ ;+& () o*`;-';\
               (`""--.,_0 +% @' &()\
               /-.,_    ``''--....-'`)  *
          *    /@%;o`:;'--,.__   __.'\
              ;*,&(); @ % &^;~`"`o;@();         *
              /(); o^~; & ().o@*&`;&%O\
        jgs   `"="==""==,,,.,="=="==="`
           __.----.(\-''#####---...___...-----._
         '`         \)_`"""""`
                 .--' ')
               o(  )_-\
                 `"""` `
```

My Advent of Code solutions, [in various languages](doc/PUZZLES.md).

`aoc.sh` wraps the language specific tools to have somewhat unified API for
testing and developing puzzles. `./aoc.sh $language $day $year` sets up files
for a new puzzle & starts an interactive dev session if the given language has
such capabilities. `./aoc.sh $language $day $year test` can be used to verify
solutions.

Puzzles are self-contained scripts (except for Rust - all puzzles are compiled
to a single executable). Building the tooling sometimes is more fun than
dealing with puzzles themselves, so a bash duct tape is holding this project
together. [DEV.md](doc/DEV.md) has more detailed information about how this
all works together.

All puzzles read their input from `resources/$year/day$day.txt`, all puzzles
(except for Clojure, which uses `clojure.test`) are expected to print out
their solutions which are then compared to `resources/$year/solutions/day$day.txt`.

______________________________________________________________________

2017 - ∞ [skazhy](https://karlis.me)
