{-|
Module      : Day15
Description : Day 15: Rambunctious Recitation
Tags        : incomplete, slow

<https://adventofcode.com/2020/day/15>
-}

module Day15 where

import Advent
import Prelude hiding (lookup)
import Data.IntMap (IntMap, fromList, lookup, insert, (!))

seenMap :: [Int] -> IntMap Int
seenMap = fromList . flip zip [0..]

-- | Finds the next van Eck sequence element.
nextElement :: [Int] -> IntMap Int -> Int -> Int
nextElement ves seen i =
    case lookup (head ves) seen of
         Just _ -> i - (seen ! head ves)
         Nothing -> 0

runVanEckSequence :: Int -> [Int] -> Int
runVanEckSequence lim input =
    go (reverse input) (seenMap $ init input) (length input - 1) where
    go ves seen i | i > lim - 2 = head ves
                  | otherwise = go (nextElement ves seen i : ves) (insert (head ves) i seen) (i + 1)

main = do
    input <- parsedInput (2020, 15) intSequence
    print $ runVanEckSequence 2020 input
    --- print $ runVanEckSequence 30000000 input
