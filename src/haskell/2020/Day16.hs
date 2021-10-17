{-|
Module      : Day16
Description : Day 16: Ticket Translation

<https://adventofcode.com/2020/day/16>
-}

module Day16 where

import Advent

import Control.Monad
import Data.Bifunctor (bimap, second)
import Data.List (intersect, isPrefixOf, transpose, find, delete)

parseRange :: String -> (Int -> Bool)
parseRange = inRange . bimap read (read . tail) . span (/= '-')

parseRanges :: String -> (Int -> Bool)
parseRanges s =
    let [r, _, r2] = words s
    in liftM2 (||) (parseRange r) (parseRange r2)

parseRangeRow :: String -> (String, Int -> Bool)
parseRangeRow = second (parseRanges . tail) . span (/= ':')

anyFieldMatches :: [(String, Int -> Bool)] -> Int -> Bool
anyFieldMatches checkers = or . mapM snd checkers

-- Puzzle 1: Finding invalid elements in each row

puzzle1 :: [(String, Int -> Bool)] -> [[Int]] -> Int
puzzle1 fields = sum . concatMap (filter (not . anyFieldMatches fields))

-- Puzzle 2: Solving the train ticket sudoku

fieldChoices :: [(String, Int -> Bool)] -> Int -> [String]
fieldChoices fields i = map fst $ filter (\(_, m) -> m i) fields

uniqueFieldChoices :: [(String, Int -> Bool)] -> [[Int]] -> [[String]]
uniqueFieldChoices fields =
    map (foldl1 intersect) . transpose . map (map (fieldChoices fields))

deleteFieldAndIdx :: Int -> String -> [(Int, [String])] -> [(Int, [String])]
deleteFieldAndIdx idx r = filter ((/= idx) . fst) . map (second (delete r))

fieldIndexes :: [(String, Int -> Bool)] -> [[Int]] -> [(Int, String)]
fieldIndexes fields =
    go . zip [0..] . uniqueFieldChoices fields where
    go cs =
        case find ((== 1) . length . snd) cs of
            Just (idx, [r]) -> (idx, r) : go (deleteFieldAndIdx idx r cs)
            _ -> []

departureFields :: [Int] -> [(Int, String)] -> [Int]
departureFields ticket =
    map ((ticket !!) . fst) . filter (("departure" `isPrefixOf`) . snd)

puzzle2 :: [(String, Int -> Bool)] -> [Int] -> [[Int]] -> Int
puzzle2 fields ticket =
    product . departureFields ticket . fieldIndexes fields . filter (all matcher) where
    matcher = anyFieldMatches fields

main = do
    (fs:ticket:nearby:_) <- parsedInput (2020, 16) (groupedLines . lines)
    let fields = map parseRangeRow fs
    print $ puzzle1 fields (map intSequence (tail nearby))
    print $ puzzle2 fields (intSequence (last ticket)) (map intSequence (tail nearby))
