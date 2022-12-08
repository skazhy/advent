let s:srcPattern =  '[a-z|/]\+\(\d\+\)/[a-z|_]\+\(\d\+\).[a-z]\+'

function! OpenAocResource(path)
  let l:srcPath = expand('%')
  if match(l:srcPath, s:srcPattern) > -1
    execute 'split ' .  substitute(l:srcPath, s:srcPattern, a:path, "")
  endif
endfunction

command! -nargs=* AocInput call OpenAocResource('resources/\1/day\2.txt')
command! -nargs=* AocSolution call OpenAocResource('resources/\1/solutions/day\2.txt')
command! -nargs=* AocTest call OpenAocResource('test/advent/\1/test_day\2.clj')
