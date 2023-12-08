{-|
Module      : Day22
Description : Day 22: Crab Combat
Tags        : slow

<https://adventofcode.com/2020/day/22>
-}

module Day22 where

import Advent

import Data.Bifunctor (bimap)
import Data.List (elemIndex)

type Decks = ([Int], [Int])

parseData :: [String] -> Decks
parseData = bimap (map read . tail) (map read . drop 2) . span (/= "")

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

-- Puzzle 1: Crab combat without recursive combat and infinite loop prevention

turn :: Decks -> Decks
turn (x:xs, y:ys)
    | x > y = (xs ++ [x,y], ys)
    | otherwise = (xs, ys ++ [y,x])
turn e = e

-- |Plays turns until one of the players has an empty hand
playGame :: Decks -> Decks
playGame = head . dropWhile (not . all null) . iterate turn

-- Puzzle 2: Crab combat with recursive combat and infinite loop prevention

canRecurse :: [Int] -> Bool
canRecurse (x:xs) = x <= length xs

subdeck :: [Int] -> [Int]
subdeck (x:xs) = take x xs

turn2 :: [Decks] -> [Decks]
turn2 t@(h@(x:xs, y:ys):hs)
    | not $ null $ elemIndex h hs = (fst h, []) : t
    | (uncurry (&&) . bimap canRecurse canRecurse) h =
        case (playGame2 . pure . bimap subdeck subdeck) h of
             ([], _) -> (xs, ys ++ [y,x]) : t
             _  -> (xs ++ [x,y], ys) : t
    | otherwise = turn h : t
turn2 e = e

playGame2 :: [Decks] -> Decks
playGame2 [] = ([], [])
playGame2 h@(x:_)
    | null (fst x) || null (snd x) = x
    | otherwise = playGame2 $ turn2 h

main = do
    input <- parsedInput (2020, 22) (parseData . lines)
    print $ (uncurry (+) . bimap score score . playGame) input
    print $ (uncurry (+) . bimap score score . playGame2 . pure) input
