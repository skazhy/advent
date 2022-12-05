{-|
Module      : Day5
Description : Day 5: Supply Stacks

<https://adventofcode.com/2022/day/5>
-}

module Day5 where

import Advent
import Data.List (transpose)
import Data.Bifunctor (bimap)
import Data.Map (Map, adjust, fromList, toList, (!))

type Cmd = (Int, Int, Int)
type Stacks = Map Int String

parseStackRow :: String -> String
parseStackRow [] = []
parseStackRow (_:c:xs) = c : parseStackRow (drop 2 xs)

parseStackRows :: [String] -> Stacks
parseStackRows =
    fromList . zip [1..] . reverse . map (dropWhile (== ' ')) . transpose . map (reverse . parseStackRow)

parseCmdRow :: String -> Cmd
parseCmdRow cmd =
    (read $ s !! 1, read $ s !! 3, read $ s !! 5)
    where s = words cmd

parseInput :: [String] -> (Stacks, [Cmd])
parseInput =
    bimap (parseStackRows . init) (map parseCmdRow . tail) . break  (== "")

runCmd :: (String -> String) -> Stacks -> Cmd -> Stacks
runCmd reorder stacks (i,from,to) =
    adjust (drop i) from $ adjust (\x -> reorder (take i $  stacks ! from) ++ x) to stacks

topCrates :: Stacks -> String
topCrates = foldr (\a b -> head a : b) ""

main = do
    (stack,cmds) <- parsedInput (2022, 5) (parseInput . lines)
    putStrLn $ topCrates $ foldl (runCmd reverse) stack cmds
    putStrLn $ topCrates $ foldl (runCmd id) stack cmds
