{-|
Module      : Day1
Description : Day 1: Sonar Sweep

<https://adventofcode.com/2021/day/1>
-}

module Day1 where

import Advent
import Data.List (length, mapAccumL)

loadInput = parsedInput (2021, 1) intLines

increaseCount :: [Int] -> Int
increaseCount (x:xs) = length $ filter id $ snd $ mapAccumL (\a b -> (b, a < b)) x xs
increaseCount _ = 0

slidingWindows :: [Int] -> [Int]
slidingWindows input =
  zipWith3 (\a b c -> a + b + c) input (drop 1 input) (drop 2 input)

main = do
    input <- loadInput
    print $ increaseCount input
    print $ increaseCount $ slidingWindows input
