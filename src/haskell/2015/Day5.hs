{-|
Module      : Day5
Description : Day 5: Doesn't He Have Intern-Elves For This?

<https://adventofcode.com/2015/day/5>
-}

module Day5 where

import Advent

import Control.Monad
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set

--- Part 1

vowels :: Set Char
vowels = Set.fromList "aeiou"

naughtySubstrings :: Set String
naughtySubstrings = Set.fromList ["ab", "cd", "pq", "xy"]

checkVowel :: Char -> Int -> Int
checkVowel c i | Set.member c vowels = i + 1
               | otherwise = i

isNiceString1 :: String -> Bool
isNiceString1 s = go s 0 0 where
    go (a:b:xs) vc dc | a == b = go (b:xs) (checkVowel a vc) (dc + 1)
                      | Set.member [a,b] naughtySubstrings = False
                      | otherwise = go (b:xs) (checkVowel a vc) dc
    go (b:xs) vc dc = go xs (checkVowel b vc) dc
    go _ vc dc = vc > 2 && dc > 0

--- Part 2

pairs :: String -> Bool
pairs s = go s Set.empty where
    go (a:b:c:xs) s | a == b && a == c && Set.member [a,a] s = go (c:xs) s
                    | a == b && a == c = go (c:xs) (Set.insert [a,a] s)
                    | Set.member [a,b] s = True
                    | otherwise = go (b:c:xs) (Set.insert [a,b] s)
    go (a:b:xs) s | Set.member [a,b] s = True
                  | otherwise = False
    go _ _ = False

matching :: String -> Bool
matching (a:b:c:xs) | a == c = True
                    | otherwise = matching (b:c:xs)
matching _ = False

main = do
    input <- parsedInput (2015, 5) lines
    print $ length $ filter isNiceString1 input
    print $ length $ filter (liftM2 (&&) pairs matching) input
