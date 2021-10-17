{-|
Module      : Day1
Description : Day 1: Report Repair

<https://adventofcode.com/2020/day/1>
-}

module Day1 where

import Advent

import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

elementResult :: [Int] -> [Int] -> Maybe Int
elementResult items els =
    let el = 2020 - sum els in
    if el `elem` items
       then Just $ foldr (*) el els
       else Nothing

tripletResult :: [Int] -> Int -> Maybe Int
tripletResult (x:xs) y =
    elementResult xs [x, y] <|> tripletResult xs y
tripletResult [] _ = Nothing

puzzle1 :: [Int] -> Maybe Int
puzzle1 items =
    asum $ fmap (elementResult items . pure) items

puzzle2 :: [Int] -> Maybe Int
puzzle2 items =
    asum $ map (tripletResult items) items

main = do
    items <- parsedInput (2020, 1) intLines
    print $ (fromMaybe 0 . puzzle1) items
    print $ (fromMaybe 0 . puzzle2) items
