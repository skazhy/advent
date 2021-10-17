{-|
Module      : Day23
Description : Day 23: Crab Cups

<https://adventofcode.com/2020/day/23>
-}

module Main where

import Advent

import Control.Applicative (liftA2)
import Data.Bifunctor (second)
import Data.List (maximum, sort)

parseInput :: String -> [Int]
parseInput = map (read . pure)

destCup :: Int -> [Int] -> Int
destCup i ixs =
    case takeWhile (< i) $ sort ixs of
        [] -> maximum ixs
        a -> maximum a

playTurn :: (Int, [Int]) -> (Int, [Int])
playTurn (curr, cups) =
    let
        (b,a) = second (drop 1) $ span (/= curr) cups
        r = drop 3 a ++ b
        d = destCup curr r
    in
        ( head (takeWhile (/= d) r ++ [d])
        , [curr] ++ takeWhile (/= d) r ++ [d] ++ take 3 a ++ drop 1 (dropWhile (/= d) r)
        )

formatResult :: Int -> [Int] -> Int
formatResult r = read . foldl1 (++) . map show . take r . drop 1 . dropWhile (/= 1) . cycle

puzzle1 :: [Int] -> Int
puzzle1 = formatResult 8 . snd . (!! 100) . iterate playTurn . liftA2 (,) head id

main = do
    input <- parsedInput (2020, 23) parseInput
    print $ puzzle1 input
