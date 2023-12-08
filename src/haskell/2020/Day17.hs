{-|
Module      : Day17
Description : Day 17: Conway Cubes
Tags        : slow

<https://adventofcode.com/2020/day/17>
-}

module Day17 where

import Advent

import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.List (concatMap, zip)
import Data.Map (Map, empty, insertWith, keys, lookup, fromList, mapWithKey)
import qualified Data.Map as M

import Data.Maybe (fromMaybe, catMaybes)

-- Parsing

gridCell :: Int -> Int -> Char -> ([Int], Bool)
gridCell y x c = ([x,y,0,0], c == '#')

parseGrid :: [String] -> Map [Int] Bool
parseGrid = fromList . concatMap (\(y, f) -> zipWith (gridCell y) [0..] f) . zip [0..]

--

neighbors :: [Int] -> ([Int] -> [Int]) -> [[Int]]
neighbors (x:y:z:w:_) wn =
    [ [x',y',z', w'] | x' <- [x-1..x+1]
                     , y' <- [y-1..y+1]
                     , z' <- [z-1..z+1]
                     , w' <- wn [w-1..w+1]
                     , (x',y',z',w') /= (x,y,z,w)
                     ]
neighbors _ _ = []

genGrid :: [Int] -> [Int] -> ([Int] -> [Int]) -> Map [Int] Bool -> Map [Int] Bool
genGrid (x:y:z:w:_) (x':y':z':w':_) wn grid =
    foldl (\a f -> insertWith (||) f False a)
    grid
    [ [x' ,y' ,z' , w'] | x' <- [x..x']
                        , y' <- [y..y']
                        , z' <- [z..z']
                        , w' <- wn [w..w']
                        ]

--

gridBounds :: Map [Int] Bool -> (Int -> Int -> Int) -> [Int]
gridBounds grid f = foldl1 (zipWith f) $ keys grid

growGrid :: ([Int] -> [Int]) -> Map [Int] Bool -> Map [Int] Bool
growGrid wn grid =
    genGrid (map (subtract 1) $ gridBounds grid min) (map (+1) $ gridBounds grid max) wn grid

activeNeighborCount :: [Int] -> Map [Int] Bool -> ([Int] -> [Int]) -> Int
activeNeighborCount cell grid wn =
    foldl (\a x -> a + fromEnum (Just True == lookup x grid)) 0 $ neighbors cell wn
    -- map (flip lookup grid) $ threeDNeighbors  cell

decide :: Map [Int] Bool -> ([Int] -> [Int]) -> [Int] -> Bool -> Bool
decide grid wn k True = liftM2 (||) (== 2) (== 3) $ activeNeighborCount k grid wn
decide grid wn k False = activeNeighborCount k grid wn == 3

conwayStep :: ([Int] -> [Int]) -> Map [Int] Bool -> Map [Int] Bool
conwayStep wn grid  =
    mapWithKey (decide grid wn) $ growGrid wn grid

runConway :: ([Int] -> [Int]) -> [String] -> Int
runConway wn = length . M.filter id . (!! 6) . iterate (conwayStep wn) . parseGrid

main = do
    input <- parsedInput (2020, 17) lines
    print $ runConway (const [0]) input
    print $ runConway id input
