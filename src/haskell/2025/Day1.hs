{-|
Module      : Day1
Description : Day 1: Secret Entrance

<https://adventofcode.com/2025/day/1>
-}

module Day1 where

import Advent

parseRotation :: String -> Int
parseRotation ('L' : c) = - (1 * read c)
parseRotation (_ : c) = read c

clamp :: Int -> Int
clamp s | s > 99 = clamp $ s - 100
        | s < 0 = clamp $ 100 + s
        | otherwise = s

clampCount :: (Int, Int) -> Int -> (Int, Int)
clampCount (acc, start) rot = go acc $ start + rot
    where go acc s -- When left rotation ends on 0, count it as a visit.
                   | s == 0 && (rot < 0) = (acc + 1, 0)
                   -- When doing left rotations with 0 as the start, do not count the
                   -- initial 0 as a visit.
                   | s < 0 && start == 0 = clampCount (acc - 1, s) 0
                   -- Basic cases for skipping over 0 and rotations within the circle.
                   | s < 0 = go (acc + 1) $ 100 + s
                   | s > 99 = go (acc + 1) $ s - 100
                   | otherwise = (acc, s)

main = do
    input <- parsedInput (2025, 1) (map parseRotation . lines)
    print $ length $ filter (== 0) $ scanl (\a x -> clamp $ a + x) 50 input
    print $ fst $ foldl clampCount (0, 50) input
