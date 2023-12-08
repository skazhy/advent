{-|
Module      : Day6
Description : Day 6: Wait For It
Tags        : slow

<https://adventofcode.com/2023/day/6>
-}

module Day6 where

import Advent
import Data.List (transpose)
import Data.Maybe (mapMaybe)

toTuple :: [a] -> Maybe (a, a)
toTuple (x:y:_) = Just (x, y)
toTuple _ = Nothing

distances :: (Int, Int) -> Int
distances (t,r) = length $ takeWhile (> r) $ dropWhile (< r) $ map (\h -> h * (t - h)) [1..t]

-- Part 1
-- Parsing data as separate columns and finding `distances` for each

parseColumns :: [String] -> [(Int, Int)]
parseColumns = mapMaybe toTuple . transpose . map (map read . tail . words)

-- Part 2
-- Treating input as single (time, maxDistance) tuple

parseRow :: String -> Int
parseRow = read . concat . tail . words

parseRows :: [String] -> Maybe (Int, Int)
parseRows (x:y:_) = Just (parseRow x, parseRow y)
parseRows _ = Nothing

main = do
    input <- parsedInput (2023, 6) lines
    print $ product $ map distances $ parseColumns input
    printMaybe $ distances <$> parseRows input
