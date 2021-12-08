function! OpenAocPuzzle(args)
  let l:srcPath = system('./aoc.sh src-path ' . a:args)
  execute 'edit +0 ' . srcPath
endfunction

function! OpenAocTest(args)
  let l:testPath = system('./aoc.sh test-path ' . a:args)
  execute 'edit +0 ' . l:testPath
endfunction

function! OpenAocSolution(args)
  let l:solPath = system('./aoc.sh solution-path ' . a:args)
  execute 'edit +0 ' . l:solPath
endfunction

function! OpenAocInput(args)
  let l:inputPath = system('./aoc.sh input-path ' . a:args)
  execute 'edit +0 ' . l:inputPath
endfunction

command! -nargs=* AocSource call OpenAocPuzzle(<q-args>)
command! -nargs=* AocTest call OpenAocTest(<q-args>)
command! -nargs=* AocInput call OpenAocInput(<q-args>)
command! -nargs=* AocSolution call OpenAocSolution(<q-args>)
