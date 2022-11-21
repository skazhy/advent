{-|
Module      : Day8
Description : Day 8: Matchsticks

<https://adventofcode.com/2015/day/8>
-}

module Day8 where

import Advent

trim = tail . init

parse :: String -> Int
parse s = go 0 $ trim s where
  go i ('\\':'x':_:_:rest) = go (i + 1) rest
  go i ('\\':_:rest) = go (i + 1) rest
  go i (_:rest) = go (i + 1) rest
  go i [] = length s - i

parse2 :: String -> Int
parse2 s = go 6 $ trim s where  -- 6: new enclosing quotes + escaped previous outer quotes
  go i ('\\':'x':_:_:rest) = go (i + 5) rest
  go i ('\\':_:rest) = go (i + 4) rest
  go i (_:rest) = go (i + 1) rest
  go i [] = i - length s

main = do
    input <- parsedInput (2015, 8) lines
    print $ sum $ map parse input
    print $ sum $ map parse2 input
