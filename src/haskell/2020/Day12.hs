{-|
Module      : Day12
Description : Day 12: Rain Risk

<https://adventofcode.com/2020/day/12>
-}

module Day12 where

import Advent

import Data.Bifunctor (bimap, first, second)

data Course =
    Course { coords :: (Int, Int)
           , direction :: (Int, Int)
           } deriving (Show)

startCourse = Course (0,0)

data Navigation = Coords | Waypoint

parseLine :: String -> (Char, Int)
parseLine (x:xs) = (x, read xs)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (x, y) = (negate y, x)

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (x, y) = (y, negate x)

moveForward :: Course -> Int -> (Int, Int)
moveForward c i =
    bimap (+ fst (direction c) * i) (+ snd (direction c) * i) (coords c)

updateCourse :: Navigation -> Course -> (Char, Int) -> Course
updateCourse Coords c ('N', i) = c { coords = second (+ negate i) (coords c) }
updateCourse Coords c ('S', i) = c { coords = second (+ i) (coords c) }
updateCourse Coords c ('E', i) = c { coords = first (+ i) (coords c) }
updateCourse Coords c ('W', i) = c { coords = first (+ negate i) (coords c) }
updateCourse _ c ('N', i) = c { direction = second (+ negate i) (direction c) }
updateCourse _ c ('S', i) = c { direction = second (+ i) (direction c) }
updateCourse _ c ('E', i) = c { direction = first (+ i) (direction c) }
updateCourse _ c ('W', i) = c { direction = first (+ negate i) (direction c) }
updateCourse _ c ('F', i) = c { coords = moveForward c i }
updateCourse _ c (_, 180) = c { direction = bimap negate negate (direction c) }
updateCourse _ c ('R', 90) = c { direction = turnRight (direction c) }
updateCourse _ c ('R', 270) = c { direction = turnLeft (direction c) }
updateCourse _ c ('L', 90) = c { direction = turnLeft (direction c) }
updateCourse _ c ('L', 270) = c { direction = turnRight (direction c) }
updateCourse _ c _ = c

navigate :: Navigation -> (Int, Int) -> [(Char, Int)] -> Int
navigate navigation dir =
    res . foldl (updateCourse navigation) (startCourse dir)
    where res x =  abs (fst $ coords x) + abs (snd $ coords x)

main = do
    input <- parsedInput (2020, 12) (map parseLine . lines)
    print $ navigate Coords (1, 0) input
    print $ navigate Waypoint (10, -1) input
