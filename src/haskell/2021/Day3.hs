{-|
Module      : Day3
Description : Day 3: Binary Diagnostic

<https://adventofcode.com/2021/day/3>
-}

module Day3 where

import Advent
import Data.Int (readBinaryInt)
import Data.Maybe (mapMaybe)
import qualified Data.Bifunctor as B
import qualified Data.Map as Map

cmpTuple :: (Int -> Int -> Bool) -> (Int, Int) -> Char
cmpTuple cmp (z,o) | cmp z o = '0'
                   | otherwise = '1'

tupleMax = cmpTuple (>)
tupleMin = cmpTuple (<)

binaryProduct :: [String] -> Int
binaryProduct = product .  mapMaybe readBinaryInt

-- Puzzle 1

emptyAcc :: [String] -> Map.Map Int (Int, Int)
emptyAcc (x:_) = Map.fromList $ zip [0..] $ replicate (length x) (0,0)
emptyAcc _ = Map.empty

foldStep :: Map.Map Int (Int, Int) -> (Int, Char) -> Map.Map Int (Int, Int)
foldStep acc (idx, '0') = Map.adjust (B.first (+1)) idx acc
foldStep acc (idx, _) =  Map.adjust (B.second (+1)) idx acc

bitCounts :: [String] -> [(Int, Int)]
bitCounts rs =
    Map.elems $ foldl (\acc r -> foldl foldStep acc $ zip [0 ..] r) (emptyAcc rs) rs

-- Puzzle 2

pickChar :: Int -> ((Int, Int) -> Char) -> Char -> [String] -> Char
pickChar idx getFn tiebreaker =
    go (0,0) where
        go (z, o) (x:xs)
          | (x !! idx) == '0' = go (z + 1, o) xs
          | otherwise = go (z , o + 1) xs
        go (z, o) _
          | z == o = tiebreaker
          | otherwise = getFn (z, o)

getRating :: ((Int, Int) -> Char) -> Char -> [String] -> String
getRating getFn tiebreaker =
    go 0 where
        go _ [r] = r
        go idx rs =
            let c = pickChar idx getFn tiebreaker rs
            in
                go (idx + 1) $ filter (\r -> r !! idx == c) rs

main = do
    input <- parsedInput (2021, 3) lines
    print $ binaryProduct $ sequence [map tupleMax, map tupleMin] $ bitCounts input
    print $ binaryProduct $ sequence [getRating tupleMin '0', getRating tupleMax '1'] input
