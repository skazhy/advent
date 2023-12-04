{-|
Module      : Day4
Description : Day 4: Scratchcards

<https://adventofcode.com/2023/day/4>
-}

module Day4 where

import Advent

import Data.Bifunctor (second)
import Data.List (intersect, tails)

splitRow :: [a] -> ([a], [a])
splitRow r | length r == 14 = second tail $ splitAt 5 r   -- test data
           | otherwise      = second tail $ splitAt 10 r  -- puzzle input

--- Part 1

points :: Int -> Int
points 0 = 0
points i = 2 ^ (i - 1)

--- Part 2

cardCount :: [Int] -> Int
cardCount [_] = 1
cardCount [] = 0
cardCount (x:xs) = 1 + sum (map cardCount $ take x $ tails xs)

main = do
    input <- parsedInput (2023, 4) (map (length . uncurry intersect . splitRow . drop 2 . words) . lines)
    print $ sum $ map points input
    print $ sum $ map cardCount $ tails input
