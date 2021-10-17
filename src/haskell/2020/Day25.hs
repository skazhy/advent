{-|
Module      : Day25
Description : Day 25: Combo Breaker

<https://adventofcode.com/2020/day/25>
-}

module Day25 where

import Advent

import Data.Bifunctor (bimap)

parseInput :: String -> [Int]
parseInput = map read . lines

incrementKeyPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
incrementKeyPair keys multipliers =
    bimap ((`mod` 20201227) . (fst multipliers *)) ((`mod` 20201227) . (snd multipliers *)) keys

findEncKey :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
findEncKey pubKeys encKeys knownKeys
    | fst pubKeys == fst knownKeys = snd encKeys
    | snd pubKeys == snd knownKeys = fst encKeys
    | otherwise =
        findEncKey (incrementKeyPair pubKeys (7,7)) (incrementKeyPair encKeys knownKeys) knownKeys

main = do
    [cardPk, doorPk] <- parsedInput (2020, 25) parseInput
    print $ findEncKey (1,1) (1,1) (cardPk, doorPk)
