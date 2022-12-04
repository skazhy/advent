{-|
Module      : Day4
Description : Day 4: Camp Cleanup

<https://adventofcode.com/2022/day/4>
-}

module Day4 where

import Advent
import Control.Monad (liftM2)
import Data.Bifunctor (bimap, second)

split :: (a -> Bool) -> [a] -> ([a], [a])
split i s = second tail $ break i s

parsePair :: String -> (Int, Int)
parsePair = bimap read read . split (== '-')

parseRow :: String -> ((Int, Int), (Int, Int))
parseRow = bimap parsePair parsePair . split (== ',')

pairMatches :: ((a,a) -> (a,a) -> Bool) -> ((a,a), (a,a)) -> Bool
pairMatches pred = liftM2 (||) (uncurry pred) (uncurry (flip pred))

--- Part 1: Finding pairs that are subsets

isSubset :: (Int, Int) -> (Int, Int) -> Bool
isSubset (a1,b1) (a2,b2) = a1 >= a2 && b1 <= b2

--- Part 2: Finding pairs that overlap

pairsOverlap :: (Int, Int) -> (Int, Int) -> Bool
pairsOverlap (a1, b1) (a2, b2) = b1 >= a2 && b1 <= b2

main = do
    input <- parsedInput (2022, 4) (map parseRow . lines)
    print $ length $ filter (pairMatches isSubset) input
    print $ length $ filter (pairMatches pairsOverlap) input
