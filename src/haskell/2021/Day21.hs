{-|
Module      : Day21
Description : Day 21: Dirac Dice

<https://adventofcode.com/2021/day/21>
-}

module Day21 where

import Advent

parseInput :: [String] -> Maybe (Int, Int)
parseInput (p1:p2:_) = Just (read $ words p1 !! 4, read $  words p2 !! 4)
parseInput _ = Nothing

wrap :: Int -> Int -> Int
wrap b i =
    if new == 0 then b else new
    where new = rem i b

playTurn :: Int -> Int -> Int
playTurn pos die =
    wrap 10 $ pos + (sum $ map (wrap 100 . (+ die)) [0..2])

playGame p1pos p2pos =
    go (p1pos, 0) (p2pos, 0) 1 where
    go (p1pos, p1score) (p2pos, p2score) die
        | (p1score + newp1pos) >= 1000 = (die + 2) * p2score
        | (p2score + newp2pos) >= 1000 = (die + 5) * (p1score + newp1pos)
        | otherwise = go (newp1pos, p1score + newp1pos) (newp2pos, p2score + newp2pos) (die + 6)
        where newp1pos = playTurn p1pos die
              newp2pos = playTurn p2pos $ die + 3

example = (4,9)

main = do
    input <- parsedInput (2021, 21) (parseInput . lines)
    printMaybe $ uncurry playGame <$> input
