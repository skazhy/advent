{-|
Module      : Day7
Description : Day 7: The Treachery of Whales

<https://adventofcode.com/2021/day/7>
-}

module Day7 where

import Advent
import Data.List (group)

frequencies :: [Int] -> [(Int, Int)]
frequencies = map (\f -> (head f, length f)) . group

travelDistance :: (Int -> Int) -> [(Int, Int)] -> Int -> Int
travelDistance fuelRate xs n =
    foldl (\acc (i,c) -> acc + fuelRate (abs  (n - i)) * c) 0 xs

fuelConsumption :: (Int -> Int) -> [Int] -> Int
fuelConsumption fuelRate input =
    let
        freqs = frequencies input
    in
        minimum $ map (travelDistance fuelRate freqs) input

main = do
    input <- parsedInput (2021, 7) intSequence
    print $ fuelConsumption id input
    print $ fuelConsumption (\n -> n * (n + 1) `div` 2) input
