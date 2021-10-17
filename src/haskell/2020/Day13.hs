{-|
Module      : Day13
Description : Day 13: Shuttle Search

<https://adventofcode.com/2020/day/13>
-}

module Day13 where

import Advent

import Data.Int (ceilDiv)
import Data.List (span)
import Data.Maybe (catMaybes)

-- Parsing

parseDepartures :: String -> [Maybe Int]
parseDepartures s =
    case span (/= ',') s of
         ("x", "") -> [Nothing]
         ("x", rest) -> Nothing : parseDepartures (tail rest)
         (n, "") -> [Just (read n)]
         (n, rest) -> Just (read n) : parseDepartures (tail rest)

-- Part 1: Closest departure

nextDeparture :: Int -> Int -> (Int, Int)
nextDeparture t interval = (interval, interval * (t `ceilDiv` interval))

findClosestDeparture :: Int -> [Int] -> Int
findClosestDeparture t =
    product . sequence [fst, subtract t . snd] . foldl1 minBySnd . map (nextDeparture t)
    where minBySnd a b
            | snd a < snd b = a
            | otherwise = b

-- Part 2: Chinese remainder theorem

collectDividers :: [Maybe Int] -> [(Int, Int)]
collectDividers = go 0 where
    go _ [] = []
    go c (Just x:xs) = (x, x - c) : go (c + 1) xs
    go c (Nothing:xs) = go (c + 1) xs

extGcd :: Int -> Int -> Int
extGcd = go 1 0 where
    go s0 _ _ 0 = s0
    go s0 s1 r0 r1 = go s1 (s0 - ((r0 `div` r1) * s1)) r1 (mod r0 r1)

chineseRemainderTheorem :: [(Int, Int)] -> Int
chineseRemainderTheorem dividers =
    let
        prod = product $ map fst dividers
    in
    (`mod` prod) $
        foldl (\acc (n, a) -> acc + (a * extGcd (prod `div` n) n * (prod `div` n)))
              0
              dividers

main = do
    (time:departures:_) <- parsedInput (2020, 13) lines
    print $ findClosestDeparture (read time) (catMaybes $ parseDepartures departures)
    print $ chineseRemainderTheorem $ collectDividers $ parseDepartures departures
