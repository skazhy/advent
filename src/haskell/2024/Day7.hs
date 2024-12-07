{-|
Module      : Day7
Description : Day 7: Bridge Repair
Tags        : slow

<https://adventofcode.com/2024/day/7>
-}

module Day7 where

import Advent

mul :: Int -> Int -> Int
mul 0 b = b
mul a b = a * b

conc :: Int -> Int -> Int
conc 0 b = b
conc a b = read $ show a ++ show b

parseString :: String -> (Int, [Int])
parseString s =
  (read $ take (length h - 1) h, map read t)
  where
    (h:t) = words s

validEquation :: [Int -> Int -> Int] -> (Int, [Int]) -> Bool
validEquation ops (a, ts) = go 0 ts
  where
    go x [t] = any (\op -> a == op x t) ops
    go x (t : ts) = any (\op -> a >= op x t && go (op x t) ts) ops
    go _ _ = False

main = do
  input <- parsedInput (2024, 7) (map parseString . lines)
  print $ sum $ map fst $ filter (validEquation [mul, (+)]) input
  print $ sum $ map fst $ filter (validEquation [conc, mul, (+)]) input
