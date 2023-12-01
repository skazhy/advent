{-|
Module      : Day1
Description : Day 1: Trebuchet?!

<https://adventofcode.com/2023/day/1>
-}

module Day1 where

import Advent
import Control.Monad (liftM2)
import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

-- Part 1
-- Find first digit in line and in reversed line, then concat them and convert to int.

joinDigits :: String -> Maybe Int
joinDigits s = liftM2 (\a b -> read [a, b]) (find isDigit s) (find isDigit $ reverse s)

-- Part 2
-- Preserve all digit literals, transform all spelled digits to chars, then
-- Combine first and last char digit and convert to int.

digits :: [(String, Char)]
digits = [ ("one", '1')
         , ("two", '2')
         , ("three", '3')
         , ("four", '4')
         , ("five", '5')
         , ("six", '6')
         , ("seven", '7')
         , ("eight", '8')
         , ("nine", '9')
         ]

numericPrefix :: String -> Maybe Char
numericPrefix s = snd <$> find ((`isPrefixOf` s) . fst) digits

initDigit :: String -> Maybe Char
initDigit [] = Nothing
initDigit s | isDigit $ head s = Just $ head s
            | otherwise = numericPrefix s

joinDigits2 :: String -> Int
joinDigits2 = read . sequence [head, last] . mapMaybe initDigit . tails

main = do
    input <- parsedInput (2023, 1) lines
    print $ sum $ mapMaybe joinDigits input
    print $ sum $ map joinDigits2 input
