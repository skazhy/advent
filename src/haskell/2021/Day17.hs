{-|
Module      : Day17
Description : Day 17: Trick Shot

<https://adventofcode.com/2021/day/17>
-}

module Day17 where

import Data.Maybe (catMaybes)

type Point = (Int, Int)

runStep :: (Point, Point) -> (Point, Point)
runStep ((x, y), (vx, vy)) = ((x + vx, y + vy), (vx - signum vx, vy - 1))

above :: Point -> Point -> Bool
above (x1, y1) (x2, y2) = x1 <= x2 && y2 <= y1

maxY :: (Point, Point) -> Point -> Maybe Int
maxY (upper, lower) v =
    go $ reverse $ takeWhile ((`above` lower) . fst) $ iterate runStep ((0,0), v) where
    go ((coord,_):t) | upper `above` coord = Just $ maximum $ map (snd . fst) t
    go _ = Nothing

runMaxY :: (Point, Point) -> [Int]
runMaxY bounds@(_, (bx, by)) =
    catMaybes [ maxY bounds (vx,vy) | vx <- [0..bx], vy <- [by..(negate by)]]

main = do
    let input = ((257, -57), (286, -101))
    print $ maximum $ runMaxY input
    print $ length $ runMaxY input
