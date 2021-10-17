{-|
Module      : Day1
Description : Day 1: Chronal Calibration

<https://adventofcode.com/2018/day/1>
-}

module Day1 where

import Advent
import Data.Set (Set, empty, member, insert)

parseNum :: String -> Int
parseNum ('+':r) = read r
parseNum r = read r

firstRepeated :: Set Int -> [Int] -> Maybe Int
firstRepeated seen (x:xs)
    | member x seen = Just x
    | otherwise = firstRepeated (insert x seen) xs
firstRepeated _ _ = Nothing

main = do
    input <- parsedInput (2018, 1) (map parseNum . lines)
    print $ sum input
    printMaybe $ firstRepeated empty $ scanl (+) 0 $ cycle input
