{-|
Module      : Day1
Description : Day 1: Historian Hysteria

<https://adventofcode.com/2024/day/1>
-}

module Day1 where

import Advent
import Data.List (sort)
import Data.IntMap (IntMap, alter, empty, findWithDefault)

parseRow :: String -> (Int, Int)
parseRow s = (a, b) where
  [a, b] = map read $ words s

puzzle1 :: [(Int, Int)] -> Int
puzzle1 a = sum $ zipWith ((abs .) . subtract) (sort $ map fst a) (sort $ map snd a)

--- Puzzle 2

increment :: Maybe Int -> Maybe Int
increment Nothing = Just 1
increment (Just x) = Just $ x + 1

puzzle2 :: [(Int, Int)] -> Int
puzzle2 a = sum $ map (\(x, _) -> x * findWithDefault 0 x counts) a where
  counts = foldl (flip (alter increment)) empty $ map snd a

main = do
    input <- parsedInput (2024, 1) (map parseRow . lines)
    print $ puzzle1 input
    print $ puzzle2 input
