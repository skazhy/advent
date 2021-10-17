{-|
Module      : Day6
Description : Day 6: Custom Customs

<https://adventofcode.com/2020/day/6>
-}

module Day6 where

import Advent
import Data.List (intersect)
import Data.Set (fromList)

main = do
    input <- parsedInput (2020, 6) (groupedLines . lines)
    print $ (sum . map (length . fromList . foldl1 (++))) input
    print $ (sum . map (length . foldl1 intersect)) input
