{-|
Module      : Day4
Description : Day 4: Printing Department

<https://adventofcode.com/2025/day/4>
-}

module Day4 where

import Advent

import Data.Set (Set, fromList, member, toList, delete)

mkCoords :: (Int, Int) -> (Int, Int) -> (Int, Int)
mkCoords (y, x) (dy, dx) = (y + dy, x + dx)

neighbors = [ (-1, -1), (-1, 0), (-1, 1)
            , (0, -1), (0, 1)
            , (1, -1), (1, 0), (1, 1)
            ] :: [(Int, Int)]

mkGrid :: [String] -> Set (Int, Int)
mkGrid rows =
  fromList [ (y, x)
           | (y, row) <- zip [0..] rows
           , (x, c)  <- zip [0..] row
           , c == '@'
           ]

neighborCount :: Set (Int, Int) -> (Int, Int) -> Int
neighborCount s n = length $ filter ((`member` s) . mkCoords n) neighbors

removedRolls :: Set (Int, Int) -> [(Int, Int)]
removedRolls grid = filter ((< 4) . neighborCount grid) $ toList grid

allRemovedCount :: Set (Int, Int) -> Int
allRemovedCount s =
  case removedRolls s of
    [] -> 0
    r -> length r + allRemovedCount (foldr delete s r)

main = do
    input <- parsedInput (2025, 4) (mkGrid . lines)
    print $ length $ removedRolls input
    print $ allRemovedCount input
