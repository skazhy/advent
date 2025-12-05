{-|
Module      : Day5
Description : Day 5: Cafeteria

<https://adventofcode.com/2025/day/5>
-}

module Day5 where

import Advent

import Data.Bifunctor (bimap)
import Data.List (sort)

mkRange :: String -> (Int, Int)
mkRange = bimap read (read . tail) . break (== '-')

parseInput :: [String] -> ([(Int, Int)], [Int])
parseInput = bimap (map mkRange)  (map read . tail) . break (== "")

--- Part 1
--- Count all ingredients that are present in a range

countFreshIngredients :: [(Int, Int)] -> [Int] -> Int
countFreshIngredients ranges ingredients =
  length $ filter isFresh ingredients
   where isFresh x = any (`inRange` x) ranges

--- Part 2
--- Get the total size of all ranges by merging overlapping ranges

rangesOverlap :: (Int, Int) -> (Int, Int) -> Bool
rangesOverlap (a, b) (x, y) = x <= b

mergeRanges :: (Int, Int) -> (Int, Int) -> (Int, Int)
mergeRanges (a, b) (x, y) = (a, max b y)

mergeAll  :: [(Int, Int)] -> [(Int, Int)]
mergeAll [] = []
mergeAll ranges = l : acc
  where (x:xs) = sort ranges
        (acc, l) = foldl (\(acc, r0)  r -> case rangesOverlap r0 r of
            True -> (acc, mergeRanges r0 r)
            False -> (r0 : acc, r))
            ([], x)
            xs

main = do
    (ranges, ingredients) <-parsedInput (2025, 5) (parseInput . lines)
    print $ countFreshIngredients ranges ingredients
    print $ sum $ map (\(x,y) -> (y - x) + 1) $ mergeAll ranges
