{-|
Module      : Day11
Description : Day 11: Seating System

<https://adventofcode.com/2020/day/11>
-}

module Day11 where

import Advent

import Control.Monad (join)
import Data.Map (Map, fromList, lookup, mapWithKey)

data Seat = Free | Taken deriving (Show, Enum, Eq)
type FloorPlan = Map [Int] (Maybe Seat)

neighbors = [ [-1, -1], [-1, 0], [-1, 1]
            , [0, -1], [0, 1]
            , [1, -1], [1, 0], [1, 1]
            ] :: [[Int]]

-- Parsing

parseChar :: Char -> Maybe Seat
parseChar '#' = Just Taken
parseChar 'L' = Just Free
parseChar _ = Nothing

mkFloorPlan :: [String] -> FloorPlan
mkFloorPlan rows =
    fromList $ zip [0..] rows >>= uncurry mapEntries
    where mapEntries i = map (\(j, c) -> ([i, j], parseChar c)) . zip [0..]

-- Part 1: Adjacent neighbords

neighborCount :: FloorPlan -> [Int] -> Int
neighborCount fp coords =
    foldl count 0 $ fmap (\n -> join $ Data.Map.lookup (zipWith (+) n coords) fp) neighbors
    where count a Nothing = a
          count a (Just s) = a + fromEnum s

updateSeat1 :: Seat -> FloorPlan -> [Int] -> Seat
updateSeat1 Free fp coords | neighborCount fp coords == 0 = Taken
                           | otherwise = Free
updateSeat1 Taken fp coords | neighborCount fp coords > 3 = Free
                            | otherwise = Taken

-- Part 2: Visible neighbors

findNeighbor :: FloorPlan -> [Int] -> [Int] -> Maybe Seat
findNeighbor fp coords delta =
    case Data.Map.lookup (zipWith (+) coords delta) fp of
         Just Nothing -> findNeighbor fp (zipWith (+) coords delta) delta
         Nothing -> Nothing
         Just a -> a

neighborCount2 :: FloorPlan -> [Int] -> Int
neighborCount2 fp coords =
    foldl count 0 $ fmap (findNeighbor fp coords) neighbors
    where count a Nothing = a
          count a (Just s) = a + fromEnum s

updateSeat2 :: Seat -> FloorPlan -> [Int] -> Seat
updateSeat2 Free fp coords | neighborCount2 fp coords == 0 = Taken
                           | otherwise = Free
updateSeat2 Taken fp coords | neighborCount2 fp coords > 4 = Free
                            | otherwise = Taken

--

findEquilibrium :: (Seat -> FloorPlan -> [Int] -> Seat) -> FloorPlan -> Int
findEquilibrium updater fp =
    let new = mapWithKey (\k v -> fmap (\s -> updater s fp k) v) fp
    in if new == fp
        then foldl (\a i -> a + maybe 0 fromEnum i) 0 new
        else findEquilibrium updater new

main = do
    input <- parsedInput (2020, 11) (mkFloorPlan . lines)
    print $ findEquilibrium updateSeat1 input
    print $ findEquilibrium updateSeat2 input
