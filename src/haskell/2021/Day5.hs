{-# LANGUAGE TupleSections #-}

{-|
Module      : Day5
Description : Day 5: Hydrothermal Venture

<https://adventofcode.com/2021/day/5>
-}

module Day5 where

import Advent
import Data.Map (Map, filter, fromListWith)

range :: Int -> Int -> [Int]
range a b
    | a < b = [a..b]
    | otherwise = [b..a]

frequencies :: Ord a => [a] -> Map a Int
frequencies = fromListWith (+) . flip zip (repeat 1)

makeLine :: [Int] -> [Int] -> [(Int,Int)]
makeLine (x0:y0:_) (x1:y1:_)
    | x0 == x1 = map (x0,) $ range y0 y1
    | y0 == y1 = map (,y0) $ range x0 x1
    --- TODO: find a more generic way to write these.
    | x0 > x1 && y0 > y1 = zip (reverse [x1..x0]) (reverse [y1..y0])
    | x0 > x1 = zip (reverse [x1..x0]) [y0..y1]
    | y0 > y1 = zip [x0..x1] (reverse [y1..y0])
    | otherwise = zip [x0..x1] [y0..y1]
makeLine _ _ = []

parseLine :: String -> [(Int, Int)]
parseLine s =
    let
       (x:_:y:_) = words s
    in
        makeLine (intSequence x) (intSequence y)

countDangerousPoints :: [[(Int, Int)]] -> Int
countDangerousPoints = length . Data.Map.filter (> 1) . frequencies . concat

straightLine :: [(Int, Int)] -> Bool
straightLine ((x0,y0):(x1,y1):_) = x0 == x1 || y0 == y1
straightLine _ = True

main = do
    input <- parsedInput (2021, 5) (map parseLine . lines)
    print $ countDangerousPoints $ Prelude.filter straightLine input
    print $ countDangerousPoints input
