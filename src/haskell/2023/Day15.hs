{-|
Module      : Day15
Description : Day 15: Lens Library

<https://adventofcode.com/2023/day/15>
-}

module Day15 where

import Advent
import Data.Bifunctor (second)
import Data.Char (ord, isAlpha)
import Data.Map (Map, alter, empty, foldlWithKey)

splitInput :: String -> [String]
splitInput [] = []
splitInput s = uncurry (:) $ second (splitInput . drop 1) $ break (== ',') s

encode :: String -> Int
encode = foldl (\acc c -> rem ((acc + ord c) * 17) 256) 0

--- Part 2
runOp :: String -> String -> Maybe [(String, Int)] -> Maybe [(String, Int)]
runOp k ('=':v) (Just l) | elem k $ map fst l = Just $ takeWhile ((/= k) . fst) l ++
                                                       ((k, read v) : tail (dropWhile ((/= k) . fst) l))
                         | otherwise = Just $ l ++ [(k, read v)]
runOp k ('=':v) Nothing = Just [(k, read v)]
runOp k "-" (Just l) = Just $ filter((/= k) . fst) l
runOp _ _ _ = Nothing

stepFn :: Map Int [(String, Int)] -> String -> Map Int [(String, Int)]
stepFn acc f = alter (runOp label rest) (encode label) acc where
  (label, rest) = span isAlpha f

results :: Map Int [(String, Int)] -> Int
results = foldlWithKey (\acc k v -> acc + sum (zipWith (\a b -> a * snd b * (k + 1)) [1..] v)) 0

main = do
    input <- parsedInput (2023, 15) splitInput
    print $ sum $ map encode input
    print $ results $ foldl stepFn empty input
