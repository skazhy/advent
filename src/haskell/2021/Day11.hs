{-|
Module      : Day11
Description : Day 11: Dumbo Octopus

<https://adventofcode.com/2021/day/11>
-}

module Day11 where

import Advent
import Data.Bifunctor (bimap, first, second)
import Data.Int (readChar)
import Data.Map (Map, map, filter, fromList, toList, elems, update, insert)

type Grid = Map (Int, Int) Int

gridMap :: [String] -> Grid
gridMap input =
  fromList [((x,y), readChar $ input !! x !! y) | x <- [0..length input - 1],
                                                  y <- [0..length (head input) - 1]]

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords = sequence [ first (subtract 1)
                          , second (subtract 1)
                          , first (+1)
                          , second (+1)
                          , bimap (subtract 1) (subtract 1)
                          , bimap (subtract 1) (+1)
                          , bimap (+1) (subtract 1)
                          , bimap (+1) (+1)
                          ]

flashCoords :: (Int, Int) -> Grid -> Grid
flashCoords coords grid =
  foldl (flip $ update updateNeighbor) (insert coords 0 grid) (neighborCoords coords) where
    updateNeighbor 0 = Just 0
    updateNeighbor x = Just (x + 1)

flash :: Grid -> Grid
flash grid =
  case toList $ Data.Map.filter (> 9) grid of
    ((f,_):_) -> flash $ flashCoords f grid
    _ -> grid

main = do
    input <- parsedInput (2021, 11) (iterate (flash . Data.Map.map (+1)) . gridMap . lines)
    print $ foldl (\acc i -> acc + length (Data.Map.filter (==0) i)) 0 $ take 101 input
    print $ length $ takeWhile (not . all (==0) . elems) input
