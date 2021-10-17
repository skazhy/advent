{-|
Module      : Day9
Description : Day 9: Encoding Error

<https://adventofcode.com/2020/day/9>
-}

module Day9 where

import Advent
import Data.Maybe (fromMaybe)
import Control.Monad

numberEquals :: Int -> [Int] -> Bool
numberEquals _ [] = False
numberEquals x (y:ys)
    | x == y = True
    | otherwise = numberEquals x ys

hasMatchingNumbers :: Int -> [Int] -> Bool
hasMatchingNumbers _ [] = False
hasMatchingNumbers target (x:xs)
    | numberEquals (target - x) xs = True
    | otherwise = hasMatchingNumbers target xs

findInvalidNumber :: Int -> [Int] -> Maybe Int
findInvalidNumber _ [] = Nothing
findInvalidNumber len ns =
    if hasMatchingNumbers (ns !! len) (take len ns)
    then findInvalidNumber len $ tail ns
    else Just (ns !! len)

breakEncryption :: [Int] -> [Int] -> Int -> Maybe Int
breakEncryption _ [] _ = Nothing
breakEncryption queue ns target
    | sum queue == target = Just $ maximum queue + minimum queue
    | sum queue > target = breakEncryption (tail queue) ns target
    | sum queue < target = breakEncryption (queue ++ [head ns]) (tail ns) target

main = do
    input <- parsedInput (2020, 9) intLines
    print $ (fromMaybe 0 . findInvalidNumber 25) input
    print $ (fromMaybe 0 . (breakEncryption [] input <=< findInvalidNumber 25)) input
