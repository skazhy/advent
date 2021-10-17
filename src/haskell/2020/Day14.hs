{-# LANGUAGE LambdaCase #-}

{-|
Module      : Day14
Description : Day 14: Docking Data

<https://adventofcode.com/2020/day/14>
-}

module Day14 where

import Advent

import Control.Applicative (liftA2)
import Data.Bits ((.|.), (.&.), clearBit, setBit)
import Data.Int (readBinaryInt)
import Data.Map (Map, empty, insert)
import Data.List (isPrefixOf, subsequences)

-- Parsing

parseMask :: String -> String
parseMask = last . words

parseMem :: String -> (Int, Int)
parseMem s =
    let
        (mem:_:v:_) = words s
    in
        (read $ takeWhile (/= ']') $ drop 4 mem, read v)

type Memory = Map Int Int

runMaskedOps :: (String -> a) -> (Memory -> (Int, Int) -> a -> Memory) ->  [String] -> Memory
runMaskedOps _ _ [] = empty
runMaskedOps prepareMask updateMemory (l:ls) =
    go empty (prepareMask $ parseMask l) ls where
    go res _ [] = res
    go res m (x:xs) | "mask" `isPrefixOf` x = go res (prepareMask $ parseMask x) xs
                    | otherwise = go (updateMemory res (parseMem x) m) m xs

-- Puzzle 1: Applying masks to values

readMask :: Char -> String -> Maybe Int
readMask c = readBinaryInt . map (\case 'X' -> c; d -> d)

applyMaskFn :: String -> Maybe (Int -> Int)
applyMaskFn s = liftA2 (\m m' -> (m' .|.) . (m .&.)) (readMask '1' s) (readMask '0' s)

insertMaskedValue :: Memory  -> (Int, Int) -> Maybe (Int -> Int) -> Memory
insertMaskedValue acc (k, v) maskFn =
    maybe acc (\v' -> insert k v' acc) (maskFn <*> pure v)

-- Puzzle 2: Floating bit masks

maskAddress :: Int -> String -> [Int]
maskAddress start mask' = go 0 start (reverse mask') [] where
    go _ res [] floating = map (foldl setBit res) $ subsequences floating
    go i res ('1':xs) floating = go (i + 1) (setBit res i) xs floating
    go i res ('0':xs) floating = go (i + 1) res xs floating
    go i res (_:xs) floating = go (i + 1) (clearBit res i) xs (i: floating)

insertInMaskedAddrs :: Memory -> (Int, Int) -> String -> Memory
insertInMaskedAddrs acc (k, v) =
    foldl (\a k' -> insert k' v a) acc . maskAddress k

main = do
    input <- parsedInput (2020, 14) lines
    print $ (sum . runMaskedOps applyMaskFn insertMaskedValue) input
    print $ (sum . runMaskedOps id insertInMaskedAddrs) input
