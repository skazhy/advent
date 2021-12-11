{-|
Module      : Day11
Description : Day 11: Dumbo Octopus

<https://adventofcode.com/2021/day/11>
-}

module Day11 where

import Advent
import Data.Int (readChar)
import Data.Map (Map, map, filter, fromList, toList, elems, update, insert)

type Grid = Map (Int, Int) Int

gridMap :: [String] -> Grid
gridMap input =
  fromList [((x,y), readChar $ input !! x !! y) | x <- [0..length input - 1]
                                                , y <- [0..length (head input) - 1]]

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x, y) = [(x + x0, y + y0) | x0 <- [-1..1]
                                          , y0 <- [-1..1]
                                          , (x0,y0) /= (0,0)]

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
