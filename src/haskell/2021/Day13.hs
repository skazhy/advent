{-|
Module      : Day13
Description : Day 13: Transparent Origami

<https://adventofcode.com/2021/day/13>
-}

module Day13 where

import Advent
import Data.Bifunctor (bimap)
import Data.List (elemIndex, isPrefixOf, partition)
import Data.Maybe (mapMaybe)
import Data.Set (fromList, member)

data Fold = X | Y deriving (Eq, Ord, Show)

parseCoord :: String -> Maybe (Int, Int)
parseCoord s = bimap read (read . tail) . (`splitAt` s) <$> elemIndex ',' s

parseFold :: String -> (Fold, Int)
parseFold s
    | s !! 11 == 'x' = (X, read $ drop 13 s)
    | otherwise = (Y, read $ drop 13 s)

parseInput :: [String] -> ([(Int, Int)], [(Fold, Int)])
parseInput =
    bimap (mapMaybe parseCoord) (map parseFold) . partition (not . isPrefixOf "fold")

maxCoord :: Fold -> [(Fold, Int)] -> Int
maxCoord f = ( * 2) . snd . head . filter ((== f) . fst)

makeGrid :: [(Fold, Int)] -> [(Int, Int)] -> [[Bool]]
makeGrid folds coords =
    map (\y -> [member (x,y) coordSet | x <- [0..(maxCoord X folds)]]) [0..(maxCoord Y folds)]
    where coordSet = fromList coords

foldLine :: (b -> b -> c) -> Int -> [b] -> [c]
foldLine zipper a rows = zipWith zipper (take a rows) (reverse (drop (a + 1) rows))

fold :: [[Bool]] -> (Fold, Int) -> [[Bool]]
fold grid (X, a) = map (foldLine (||) a) grid
fold grid (Y, a) = foldLine (zipWith (||)) a grid

runFolds :: [(Int, Int)] -> [(Fold, Int)] -> Int -> [[Bool]]
runFolds coords folds foldCount =
    foldl fold (makeGrid folds coords) (take foldCount folds)

drawGrid :: [[Bool]] -> String
drawGrid = unlines . map (concatMap (\x -> if x then "#" else "."))

main = do
    (coords,folds) <- parsedInput (2021, 13) (parseInput . lines)
    print $ length $ concatMap (filter id) $ runFolds coords folds 1
    putStr $ drawGrid $ runFolds coords folds (length folds)
