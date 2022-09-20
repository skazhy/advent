{-|
Module      : Day3
Description : Day 3: Perfectly Spherical Houses in a Vacuum

<https://adventofcode.com/2015/day/3>
-}

module Day3 where

import Advent

import Data.Bifunctor (first, second)
import qualified Data.Set as Set

updateCoords :: Char -> (Int, Int) -> (Int, Int)
updateCoords '>' = first (+ 1)
updateCoords '<' = first (subtract 1)
updateCoords '^' = second (+ 1)
updateCoords _ = second (subtract 1)

deliverPresents :: String -> Int
deliverPresents s = Set.size $ go s (0,0) Set.empty where
    go (x:xs) coords acc = go xs (updateCoords x coords) (Set.insert coords acc)
    go _ coords acc = Set.insert coords acc

--- Part 2

data Active = Santa | Robo

dualDeliverPresents :: String -> Int
dualDeliverPresents s = Set.size $ go s (0,0) (0,0) Santa Set.empty where
    go (x:xs) sc rc Santa acc = go xs (updateCoords x sc) rc Robo (Set.insert sc acc)
    go (x:xs) sc rc Robo acc = go xs sc (updateCoords x rc) Santa (Set.insert rc acc)
    go _ sc rc _ acc = Set.insert rc $ Set.insert sc acc

main = do
    input <- parsedInput (2015, 3) id
    print $ deliverPresents input
    print $ dualDeliverPresents input
