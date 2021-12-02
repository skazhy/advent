{-|
Module      : Day2
Description : Day 2: Dive!

<https://adventofcode.com/2021/day/2>
-}

module Day2 where

import Advent

data Direction = Up | Down | Forward deriving (Show)

butlast :: (a, b, c) -> (a, b)
butlast (x,y,_) = (x,y)

parseRow :: String -> (Direction, Int)
parseRow s =
    let (d:a:_) = words s
    in case d of
            "forward" -> (Forward, read a)
            "up" -> (Up, read a)
            "down" -> (Down, read a)

applyCommand1 :: (Int, Int) -> (Direction, Int) -> (Int,Int)
applyCommand1 (x,y) (Up,a) = (x, y - a)
applyCommand1 (x,y) (Down,a) = (x, y + a)
applyCommand1 (x,y) (Forward,a) = (x + a, y)

applyCommand2 :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
applyCommand2 (x,y,aim) (Up,a) = (x, y, aim - a)
applyCommand2 (x,y,aim) (Down,a) = (x, y, aim + a)
applyCommand2 (x,y,aim) (Forward,a) = (x + a, y + (aim * a), aim)

main = do
    input <- parsedInput (2021, 2) (map parseRow . lines)
    print $ uncurry (*) $ foldl applyCommand1 (0,0) input
    print $ uncurry (*) $ butlast $ foldl applyCommand2 (0,0,0) input
