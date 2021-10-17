{-|
Module      : Day1
Description : Day 1: Not Quite Lisp

<https://adventofcode.com/2015/day/1>
-}

module Day1 where

import Advent

loadInput = parsedInput (2015, 1) id

floorDir :: Char -> Int
floorDir ')' = -1
floorDir '(' = 1
floorDir _ = 0

elevatorMove :: Int -> Int -> String -> Int
elevatorMove (-1) iteration _ = iteration
elevatorMove f iteration (h:rest) =
    elevatorMove (f + floorDir h) (iteration + 1) rest
elevatorMove _ iteration _ = iteration

main = do
    input <- loadInput
    print $ sum $ map floorDir input
    print $ elevatorMove 0 0 input
