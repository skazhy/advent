{-|
Module      : Day6
Description : Day 6: Tuning Trouble

<https://adventofcode.com/2022/day/6>
-}

module Day6 where

import Advent
import Data.List (findIndex, nub, tails)

uniqueOffset :: Int -> String -> Maybe Int
uniqueOffset n =
    fmap (+ n) . findIndex ((== n) . length . nub) . map (take n) . tails

main = do
    input <- parsedInput (2022, 6) id
    printMaybe $ uniqueOffset 4 input
    printMaybe $ uniqueOffset 14 input
