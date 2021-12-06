{-|
Module      : Day6
Description : Day 6: Lanternfish

<https://adventofcode.com/2021/day/6>
-}

module Day6 where

import Advent
import Data.IntMap (IntMap, elems, fromList, update)

rotate :: [Int] -> [Int]
rotate = tail <> take 1

addDescendantCount :: [Int] -> [Int]
addDescendantCount = take 6 <> return . (\l -> l !! 6 + last l) <> drop 7

runDay :: [Int] -> [Int]
runDay = addDescendantCount . rotate

frequencyAcc :: IntMap Int
frequencyAcc = fromList $ zip [0..] $ replicate 9 0

frequencies :: [Int] -> [Int]
frequencies = elems . foldl (flip . update $ return . (+ 1)) frequencyAcc

fishOnDay :: Int -> [Int] -> Int
fishOnDay n = sum . last . take (n + 1) . iterate runDay . frequencies

main = do
    input <- parsedInput (2021, 6) intSequence
    print $ fishOnDay 80 input
    print $ fishOnDay 256 input
