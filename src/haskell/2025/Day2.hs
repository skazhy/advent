{-|
Module      : Day2
Description : Day 2: Gift Shop

<https://adventofcode.com/2025/day/2>
-}

module Day2 where

import Advent

import Data.List
import Data.Bifunctor

parseRow :: String -> [(Int, Int)]
parseRow s =
  case dropWhile (== ',') s of
    "" -> []
    s' -> bimap read (read . tail) (break (== '-') w) : parseRow s''
      where (w, s'') = break (== ',') s'

--- Part 1
--- Split string in half and check that both parts are equal

invalidId1 :: String -> Bool
invalidId1 id = uncurry (==) $ splitAt (length id `div` 2) id

--- Part 2
--- partitition list in segments for all dividers for list's length, check that
--- all segments are equal

dividers :: [[Int]]
dividers = [[], [], [1], [1], [1, 2], [1], [1, 2, 3], [1], [1, 2, 4], [1, 3], [1, 2, 5], [1]]

everyEq :: [String] -> Bool
everyEq [] = True
everyEq (x:xs) = all (== x) xs

partitions :: Int -> String -> [String]
partitions n s =
  case take n s of
    "" -> []
    s' -> s' : partitions n (drop n s)

invalidId2 :: String -> Bool
invalidId2 id = elem True $ map (\n -> everyEq $ partitions n id) $ dividers !! length id


main = do
    input <- parsedInput (2025, 2) parseRow
    print $ sum $ map (sum . filter (invalidId1 . show) . uncurry enumFromTo) input
    print $ sum $ map (sum . filter (invalidId2 . show) . uncurry enumFromTo) input
