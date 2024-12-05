{-|
Module      : Day5
Description : Day 5: Print Queue

<https://adventofcode.com/2024/day/5>
-}

module Day5 where

import Advent
import Data.Bifunctor (bimap)
import Data.Map (Map, empty, alter, lookup)
import Data.Set (Set, insert, member, singleton)

-- Parsing data & creating the dataset

parseSeq :: String -> [Int]
parseSeq [] = []
parseSeq x = read (take 2 x) : parseSeq (drop 3 x)

parseRule :: String -> (Int, Int)
parseRule r = (read $ take 2 r, read $ drop 3 r)

addPageRule :: Int -> Maybe (Set Int) -> Maybe (Set Int)
addPageRule a Nothing = pure $ singleton a
addPageRule a (Just b) = pure $ insert a b

ruleMap :: [(Int, Int)] -> Map Int (Set Int)
ruleMap = foldl (\acc (a, b) -> alter (addPageRule b) a acc) empty

-- Rule lookup logic

ruleLookup :: Int -> Int -> Map Int (Set Int) -> Bool
ruleLookup a b rules = maybe False (member b) $ Data.Map.lookup a rules

middleEl :: [Int] -> Int
middleEl l = l !! (length l `div` 2)

pagesInOrder :: Map Int (Set Int) -> [Int] -> Bool
pagesInOrder rules = go where
  go (a:b:xs) = ruleLookup a b rules && go (b:xs)
  go _ = True

-- Bubble sort for part 2

bsortRun :: Map Int (Set Int) -> [Int] -> [Int]
bsortRun rules (x:y:rest)
  | ruleLookup x y rules = y : bsortRun rules (x:rest)
  | otherwise = x : bsortRun rules (y:rest)
bsortRun _ x = x

bsort :: Map Int (Set Int) -> [Int] -> [Int]
bsort rules l =
  if sorted == l
  then sorted
  else bsort rules sorted
  where sorted = bsortRun rules l

main = do
    (rules, seqs) <- parsedInput (2024, 5) $ bimap (ruleMap . map parseRule) (map parseSeq . tail) . break (== "") . lines
    print $ sum $ map middleEl $ filter (pagesInOrder rules) seqs
    print $ sum $ map (middleEl . bsort rules) $ filter (not . pagesInOrder rules) seqs
