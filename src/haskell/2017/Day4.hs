{-|
Module      : Day4
Description : Day 4: High-Entropy Passphrases

<https://adventofcode.com/2017/day/4>
-}

module Day4 where

import Advent

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

validPassphrase :: [String] -> Bool
validPassphrase s =
  go s Set.empty where
    go (x:xs) s
      | Set.member x s = False
      | otherwise = go xs $ Set.insert x s
    go _ _ = True

main = do
    input <- parsedInput (2017, 4) (map words . lines)
    print $ length $ filter validPassphrase input
    print $ length $ filter validPassphrase $ map (map sort) input
