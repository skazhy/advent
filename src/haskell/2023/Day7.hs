{-|
Module      : Day7
Description : Day 7: Camel Cards

<https://adventofcode.com/2023/day/7>
-}

module Day7 where

import Advent

import Data.List (group, sort, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ByteString (find)

--- Hand strength = 6 digit hex number where the first digit represents
--- tye type (full house, etc) and the rest digits represent individual cards in order

strengths = [ ('1', 1)
            , ('2', 2)
            , ('3', 3)
            , ('4', 4)
            , ('5', 5)
            , ('6', 6)
            , ('7', 7)
            , ('8', 8)
            , ('9', 9)
            , ('T', 10)
            , ('J', 11) -- Joker is remapped to 1 in part 2.
            , ('Q', 12)
            , ('K', 13)
            , ('A', 14)
            ]

cardStrength :: Char -> Int
cardStrength a = fromMaybe 0 $ lookup a strengths

strength :: String -> Int
strength h = sum $ zipWith (\a b -> cardStrength a * b) h [1048576, 65536, 4096, 256, 16, 1]

-- Hand strength is calculated in 2 parts:
-- We find type of hand (full house, etc) by pattern matching against sorter fn results.
-- Then we run the strength method on type + remapped hand to determine final strength.

handStrength :: (String -> [String]) -> (String -> String) -> String -> Int
handStrength sorter remapper h = go $ sorter h where
  go [_] = strength ('7': remapper h) -- 5 of a kind
  go [_, [_]] = strength ('6': remapper h) -- 4 of a kind
  go [_, _] = strength ('5': remapper h) -- full house
  go [_, [_], [_]] = strength ('4': remapper h) -- 3 of a kind
  go [_, _, _] = strength ('3' : remapper h) -- 2 pair
  go [_, _, _, _] = strength ('2' : remapper h) -- 1 pair
  go _ = strength ('1' : remapper h) -- high card

cardSort :: String -> [String]
cardSort = sortBy (\a b -> compare (length b) (length a)) . group . sort

parseLine :: (String -> [String]) -> (String -> String) -> String -> Maybe (Int, Int)
parseLine sorter remapper = go . words where
  go [h, bid] = Just (handStrength sorter remapper h, read bid)
  go _ = Nothing

handSorter :: (Int, Int) -> (Int, Int) -> Ordering
handSorter a b = compare (fst a) (fst b)

winningSum :: (String -> [String]) -> (String -> String) -> [String] -> Int
winningSum sorter mapper s =
  sum $ zipWith (*) (map snd $ sortBy handSorter $ mapMaybe (parseLine sorter mapper) s) [1..]

--- Part 2
--- Going through cards with Joker overrides

jokerRemap :: String -> String
jokerRemap [] = []
jokerRemap ('J':xs) = '1': jokerRemap xs
jokerRemap (x:xs) = x: jokerRemap xs

jokerSort :: String -> [String]
jokerSort h = go $ cardSort h where
  go [a] = ["a"] -- keep 5 of a kind as-is
  go [a, b] | elem 'J' h = ["a"] -- to 5 of a kind
  go [a, [b], [c]] | elem 'J' h = ["a", "b"] -- to 4 of a kind
  go [a, b, c] | elem 'J' a || elem 'J' b = ["a", "b"] -- to 4 of a kind
               | c == "J" = ["a", "bb"] -- to full house
  go [a, b, c, d] | elem 'J' h = ["aaa", "b", "c"] -- to 3 of a kind
  go x | elem 'J' h = ["aa", "b", "c", "d"] -- to pair
       | otherwise = x

main = do
    input <- parsedInput (2023, 7) lines
    print $ winningSum cardSort id input
    print $ winningSum jokerSort jokerRemap input
