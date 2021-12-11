{-|
Module      : Day9
Description : Day 9: Smoke Basin

<https://adventofcode.com/2021/day/9>
-}

module Day9 where

import Advent
import Data.Bifunctor (first, second)
import Data.Int (readChar)
import Data.List (sort)
import Data.Map (Map, filterWithKey, fromList, keys, lookup)
import Data.Maybe (mapMaybe)
import Data.Set (Set, difference, empty, fromList, insert, toList)

type Grid = Map (Int, Int) Int

gridMap :: [String] -> Grid
gridMap input =
  Data.Map.fromList [((x,y), readChar $ input !! x !! y) | x <- [0..length input - 1],
                                                           y <- [0..length (head input) - 1]]

--

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords = sequence [first (subtract 1), second (subtract 1), first (+1), second (+1)]

neighborValues :: Grid -> (Int, Int) -> [Int]
neighborValues grid = mapMaybe (`Data.Map.lookup` grid) . neighborCoords

lowPoints :: Grid -> Grid
lowPoints grid = filterWithKey (\coords v -> all (> v) $ neighborValues grid coords) grid

-- Puzzle 1

riskLevelSum :: Grid -> Int
riskLevelSum = foldl (\acc i -> acc + i + 1) 0 . lowPoints

-- Puzzle 2

basinNeighborSet :: Grid -> (Int, Int) -> Set (Int, Int)
basinNeighborSet grid =
  Data.Set.fromList . filter (maybe False (/= 9) . (`Data.Map.lookup` grid)) . neighborCoords

basinSizeForCoords :: Grid -> (Int, Int) -> Int
basinSizeForCoords grid coords =
  go empty [coords] where
    go visited (x:xs) =
      go (insert x visited) $  xs ++ Data.Set.toList (difference (basinNeighborSet grid x) visited)
    go visited _ = length visited

largestBasinProduct :: Grid -> Int
largestBasinProduct grid =
  product $ take 3 $ reverse $ sort $ map (basinSizeForCoords grid) $ keys $ lowPoints grid

main = do
    input <- parsedInput (2021, 9) lines
    print $ riskLevelSum $ gridMap input
    print $ largestBasinProduct $ gridMap input
