{-# LANGUAGE TupleSections #-}

{-|
Module      : Day7
Description : Day 7: Handy Haversacks

<https://adventofcode.com/2020/day/7>
-}

module Day7 where

import Advent

import Control.Applicative (liftA2)
import Data.List (nub)
import Data.Map hiding (map, drop, take, foldl, null)

type Dep = (String, Int)
type Deps = [Dep]

-- Parsing & building the parent -> children graph

parseDependencyPair :: [String] -> Dep
parseDependencyPair = liftA2 (,) (unwords . take 2 . drop 1) (read . head)

buildDeps :: [String] -> Deps
buildDeps ws
    | length ws == 4 = [parseDependencyPair ws]
    | otherwise = parseDependencyPair (take 4 ws) : buildDeps (drop 4 ws)

parseDeps :: [String] -> Deps
parseDeps ws
    | length ws == 3 = []
    | otherwise = buildDeps ws

parseRow :: String -> (String, Deps)
parseRow = liftA2 (,) (unwords . take 2) (parseDeps . drop 4) . words

-- Puzzle 1: inverted graph

inverseGraphLink :: (String, Deps) -> Map String [String]
inverseGraphLink (parent, deps)
    | null deps = empty
    | otherwise = fromList $ (, [parent]) <$> map fst deps

inverseGraph :: [(String, Deps)] -> Map String [String]
inverseGraph = foldl1 (unionWith (++)) . map inverseGraphLink

nodeParents :: String -> Map String [String] -> [String]
nodeParents w g =
    maybe [w] (concatMap ((w : ) . flip nodeParents g)) $ Data.Map.lookup w g

-- Puzzle 2: sum of children values

sumOfNode :: Map String Deps -> Dep -> Int
sumOfNode graph (w, i) = i * sumOfChildren w graph

sumOfChildren :: String -> Map String Deps -> Int
sumOfChildren word graph =
    maybe 0 (foldl (\acc d -> acc + sumOfNode graph d) 1) $ Data.Map.lookup word graph

main = do
    input <- parsedInput (2020, 7) (map (parseRow . init) . lines)
    print $ (subtract 1 . length . nub . nodeParents "shiny gold" . inverseGraph) input
    print $ (subtract 1 . sumOfChildren "shiny gold" . fromList) input
