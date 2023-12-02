{-|
Module      : Day2
Description : Day 2: Cube Conundrum

<https://adventofcode.com/2023/day/2>
-}

module Day2 where

import Advent

-- Transforms ["3",  "red", "4", "blue"] into [('r', 3), ('b', 4)]
pairs :: [String] -> [(Char, Int)]
pairs (x:y:xs) = (head y, read x) : pairs xs
pairs _ = []

validGame :: [(Char, Int)] -> Bool
validGame [] = True
validGame (('r', n):xs) | n < 13 = validGame xs
validGame (('g', n):xs) | n < 14 = validGame xs
validGame (('b', n):xs) | n < 15 = validGame xs
validGame _ = False

maxColorValue :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
maxColorValue (r,g,b) ('r', n) | r < n = (n, g, b)
maxColorValue (r,g,b) ('g', n) | g < n = (r, n, b)
maxColorValue (r,g,b) ('b', n) | b < n = (r, g, n)
maxColorValue acc _ = acc

tupleProduct :: (Int,Int, Int) -> Int
tupleProduct (a,b,c) = a * b * c

main = do
    input <- parsedInput (2023, 2) (map (pairs . drop 2 . words) . lines)
    print $ sum $ map snd $ filter (validGame . fst) $ zip input [1..]
    print $ sum $ map (tupleProduct . foldl maxColorValue (0,0,0)) input
