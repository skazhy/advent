{-|
Module      : Day3
Description : Day 3: Lobby

<https://adventofcode.com/2025/day/3>
-}

module Day3 where

import Advent

--- Drops the first item in list that's smaller than it's predecessor
dropDecreasing :: String -> String
dropDecreasing (x:y:ys) | x >= y = x : dropDecreasing (y:ys)
                        | otherwise = y:ys
dropDecreasing l = l

appendBattery :: Int -> [Char] -> Char -> [Char]
appendBattery size l x | length l < size = l ++ [x]
                       | otherwise = take size $ dropDecreasing $ l ++ [x]

main = do
    input <- parsedInput (2025, 3) lines
    print $ sum $ map (read . foldl (appendBattery 2) []) input
    print $ sum $ map (read . foldl (appendBattery 12) []) input
