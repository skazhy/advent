{-|
Module      : Day12
Description : Day 12: JSAbacusFramework.io

<https://adventofcode.com/2015/day/12>
-}

module Day12 where

import Advent

import Data.Char (isDigit)
import Data.List (takeWhile, dropWhile, isSubsequenceOf)

numChar :: Char -> Bool
numChar c = isDigit c || c == '-'

parseChunk :: String -> Int
parseChunk s =
  go s 0 where
  go [] n = n
  go (s:sx) n | numChar s = go (dropWhile numChar sx) $ n + read (s : takeWhile numChar sx)
  go (_:sx) n = go sx n

main = do
    input <- parsedInput (2015, 12) lines
    print $ sum $ map parseChunk input
