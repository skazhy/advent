{-|
Module      : Day2
Description : Day 2: Rock Paper Scissors

<https://adventofcode.com/2022/day/2>
-}

module Day2 where

import Advent
import Data.Map (Map, fromList, (!))

scores1 :: Map String Int
scores1 = fromList [ ("A X", 4)
                   , ("A Y", 8)
                   , ("A Z", 3)
                   , ("B X", 1)
                   , ("B Y", 5)
                   , ("B Z", 9)
                   , ("C X", 7)
                   , ("C Y", 2)
                   , ("C Z", 6)
                   ]

scores2 :: Map String Int
scores2 = fromList [ ("A X", 3)
                   , ("A Y", 4)
                   , ("A Z", 8)
                   , ("B X", 1)
                   , ("B Y", 5)
                   , ("B Z", 9)
                   , ("C X", 2)
                   , ("C Y", 6)
                   , ("C Z", 7)
                   ]

main = do
    input <- parsedInput (2022, 2) lines
    print $ sum $ map (scores1 !) input
    print $ sum $ map (scores2 !) input
