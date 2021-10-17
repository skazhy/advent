{-|
Module      : Day1
Description : Day 1: The Tyranny of the Rocket Equation

Puzzle from December 1, 2019 that serves as a template for new solutions.
<https://adventofcode.com/2019/day/1>
-}

module Day1 where

import Advent

loadInput = parsedInput (2019, 1) intLines

fuel :: Int -> Int
fuel = max 0 . subtract 2 . (`div` 3)

combinedFuel :: Int -> Int
combinedFuel =
    sum . takeWhile (> 0) . tail . iterate fuel

main = do
    input <- loadInput
    print $ (sum . map fuel) input
    print $ (sum . map combinedFuel) input
