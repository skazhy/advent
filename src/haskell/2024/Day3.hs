{-|
Module      : Day3
Description : Day 3: Mull It Over

<https://adventofcode.com/2024/day/3>
-}

module Day3 where

import Advent
import Control.Applicative (liftA2)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (maybe)
import Text.Read (readMaybe)

multStrings :: String -> String -> Maybe Int
multStrings a b = liftA2 (*) (readMaybe a) (readMaybe b)

-- bool: run next mult ops? string: remaining string to process, int: sum of mults
data State = State Bool Int String deriving (Show)

-- Attempts to run mul command from the beginning of remaining string.
-- Returns updated state with parsed part dropped & (optionally) added mult result
runMult :: State -> State
runMult (State False i s) = State False i (drop 4 s)
runMult (State True i s) =
  case (elemIndex ',' (drop 4 s), elemIndex ')' (drop 4 s)) of
    (Just c, Just b) | b - c > 1 && c > 0 && b < 9 ->
                       State True (maybe i (+ i) mult) (drop (b + 5) s)
                     | otherwise -> State True i (drop 4 s)
                    where
                      mult = multStrings (take c $ drop 4 s) $ take ((b - c) - 1) $ drop (c + 5) s
    _ -> State True i (drop 4 s)

runOps :: Bool -> String -> Int
runOps canSkip = go . State True 0 where
  go (State _ i []) = i
  go s@(State f i c) | "mul(" `isPrefixOf` c = go $ runMult s
                     | "don't()" `isPrefixOf` c && canSkip = go $ State False i (drop 7 c)
                     | "do()" `isPrefixOf` c && canSkip = go $ State True i (drop 4 c)
                     | otherwise = go $ State f i (tail c)

main = do
    input <- parsedInput (2024, 3) id
    print $ runOps False input
    print $ runOps True input
