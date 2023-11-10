||| Advent of Code 2017, day 1: Inverse Captcha
||| https://adventofcode.com/2017/day/1
module Main

import Data.String
import System.File

charToInt : Char -> Int
charToInt = (\x => x - 48) . ord

countDigits : (List Int -> List (Int, Int)) -> String -> Int
countDigits pairs s = sum $ map fst $ filter (uncurry (==)) $ pairs $ map charToInt $ unpack s

-- Create pairs for neighbor items, matching the last with the first one:
-- [1,2,3] => [(1,2), (2,3), (3,1)]
pairs1 : List Int -> List (Int, Int)
pairs1 [] = []
pairs1 (x :: xs) = go $ x :: (snoc xs x) where
  go : List Int -> List (Int, Int)
  go [] = []
  go (x :: xs) = zip (x :: xs) xs

-- Create pairs for elements with the element halwfay around the list
-- [1,2,3,4] => [(1,3), (2,4), (3,1), (4,2)]
pairs2 : List Int -> List (Int, Int)
pairs2 [] = []
pairs2 s = zip s $ drop (integerToNat $ div (natToInteger $ length s) 2) $ s ++ s

main : IO ()
main = do file <- readFile "resources/2017/day1.txt"
          case file of
               Right content => printLn $ sum $ map (countDigits pairs1) $ lines content
               Left err => printLn err
          case file of
               Right content => printLn $ sum $ map (countDigits pairs2) $ lines content
               Left err => printLn err
