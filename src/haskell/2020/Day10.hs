{-|
Module      : Day10
Description : Day 10: Adapter Array

<https://adventofcode.com/2020/day/10>
-}

module Day10 where

import Advent
import Data.List

sortedInput :: [Int] -> [Int]
sortedInput input =
    sort $ 0 : (maximum input + 3) : input

puzzle1 :: [Int] -> Int
puzzle1 =
    product . map length . group . sort . deltas
    where deltas (x:y:xs) = (y - x) : deltas (y : xs)
          deltas _ = []

puzzle2 :: [Int] -> Int
puzzle2 =
    -- fold to list of `(path count, item)` tuples
    fst . head . foldl consToPaths [(1, 0)] . tail
    where consToPaths paths x = ((sum . map fst . filter (branch x)) paths, x) : paths
          branch x (_,y) = y + 4 > x

main = do
    input <- parsedInput (2020, 10) (sortedInput . intLines)
    print $ puzzle1 input
    print $ puzzle2 input
