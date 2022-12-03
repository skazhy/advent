{-|
Module      : Day3
Description : Day 3: Rucksack Reorganization

<https://adventofcode.com/2022/day/3>
-}

module Day3 where

import Advent
import Data.Char (ord)
import Data.Bifunctor (first)
import Data.Set (Set, fromList, intersection, member, toList)

priority :: Char -> Int
priority c | ord c < 91 = ord c - 38  -- capital letters
           | otherwise  = ord c - 96  -- lowercase letters

--- Part 1: Finding duplicate item in the second half of the sublist

splitList :: String -> (String, String)
splitList l = splitAt (length l `div` 2) l

duplicateItem :: String -> Char
duplicateItem = head . uncurry filter . first (flip member . fromList) . splitList

--- Part 2: Finding common item in list groups

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked i l = take i l : chunked i (drop i l)

commonItem :: [String] -> Char
commonItem = head . toList . foldl1 intersection . map fromList

main = do
    input <- parsedInput (2022, 3) lines
    print $ (sum . map (priority . duplicateItem)) input
    print $ (sum . map (priority . commonItem) . chunked 3) input
