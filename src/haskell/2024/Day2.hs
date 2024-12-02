{-|
Module      : Day2
Description : Day 2: Red-Nosed Reports

<https://adventofcode.com/2024/day/2>
-}

module Day2 where

import Advent
import Data.List (reverse)

incrementList :: [Int] -> Bool
incrementList (a:b:xs) = inRange (1,3) (b - a) && incrementList (b:xs)
incrementList _  = True

safeList :: [Int] -> Bool
safeList l@(a:b:_) | a > b = incrementList $ reverse l
                   | a < b = incrementList l
                   | otherwise = False
safeList _ = False

--- Part 2 - Generate sublists with a single popped element & check those.

safeSubList :: [Int] -> Bool
safeSubList = go [] where
  go h (t:xs) | safeList (h ++ xs) = True
              | otherwise = go (h ++ [t]) xs
  go _ _ = False

main = do
    input <- parsedInput (2024, 2) (map (map read . words) . lines)
    print $ length $ filter safeList input
    print $ length $ filter safeSubList input
