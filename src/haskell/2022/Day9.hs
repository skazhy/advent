{-|
Module      : Day9
Description : Day 9: Rope Bridge

<https://adventofcode.com/2022/day/9>
-}

module Day9 where

import Advent
import Data.Bifunctor (bimap, first, second)
import Data.List (nub)

parseRow :: String -> [Char]
parseRow row = replicate (read $ drop 2 row) (head row)

updateHead :: Char -> (Int, Int) -> (Int, Int)
updateHead 'U' = second (subtract 1)
updateHead 'D' = second (+ 1)
updateHead 'L' = first (subtract 1)
updateHead 'R' = first (+ 1)

detached :: ((Int, Int) -> Int) -> (Int, Int) -> (Int, Int) -> Bool
detached fn a b = 1 < abs (fn a - fn b)

updateCoord :: Int -> Int -> Int
updateCoord head c = c + min 1 (max (-1) $ head - c)

updateTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
updateTail head tail
    | detached fst head tail || detached snd head tail =
        bimap (updateCoord (fst head)) (updateCoord (snd head)) tail
    | otherwise = tail

runKnot :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
runKnot moved tail = moved ++ [updateTail (last moved) tail]

runMove :: [[(Int, Int)]] -> Char -> [[(Int, Int)]]
runMove prevStates dir =
    newState : prevStates
    where
        newState = foldl runKnot [updateHead dir (head $ head prevStates)] (tail $ head prevStates)

runMoves :: Int -> [Char] -> Int
runMoves size = length . nub . map last . foldl runMove [replicate size (0,0)]

main = do
    input <- parsedInput (2022, 9) (concatMap parseRow . lines)
    print $ runMoves 2 input
    print $ runMoves 10 input
