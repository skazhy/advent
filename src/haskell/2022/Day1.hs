{-|
Module      : Day1
Description : Day 1: Calorie Counting

<https://adventofcode.com/2022/day/1>
-}

module Day1 where

import Advent
import Data.List (sort)
import Data.Bifunctor (bimap)

grouped :: [String] -> [Int]
grouped [] = []
grouped cals = uncurry (:) $ bimap (sum . map read) (grouped . drop 1) $ span (/= "") cals

main = do
    input <- parsedInput (2022, 1) lines
    print $ (maximum . grouped) input
    print $ (sum . take 3 . reverse . sort . grouped) input
