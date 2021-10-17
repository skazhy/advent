{-# LANGUAGE TupleSections #-}

{-|
Module      : Day20
Description : Day 20: Jurassic Jigsaw

<https://adventofcode.com/2020/day/20>
-}

module Day20 where

import Advent

import Data.Bifunctor (bimap, first, second)
import Data.Foldable (concatMap)
import Control.Monad (join, (<=<))
import Data.List (lookup, nub, transpose, uncons, intercalate)
import Data.Map (Map, alter, empty, toList)
import Data.String (unlines)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Control.Applicative (liftA2, (<|>))

type Tile2 = ((Int,Int), String)  -- angle id, edge id, edge string
type EdgeTile = (Int, (Int, Int))  -- tie id, angle id, edge id
type LocatedTile = (Int, Int)  -- tile id & angle id

parseTile :: [String] -> (Int, [String])
parseTile (h:xs) = ((read . init .last . words) h, xs)

tileAngles :: [String] -> [(Int, [String])]
tileAngles =
    zip [0..] . sequence [ id  -- 0°
                         , transpose . reverse  -- 90°
                         , reverse . map reverse  -- 180
                         , transpose . map reverse  -- 270°
                         , map reverse  -- flipped + 0°
                         , map reverse . transpose . map reverse  -- flipped + 90°
                         , reverse  -- flipped + 180°
                         , transpose  -- flipped + 270°
                         ]

tileEdges :: [String] -> [(Int, String)]
tileEdges = zip [0..] . sequence [ head  -- top
                                 , map head  -- left
                                 , map last  -- right
                                 , last  -- bottom
                                 ]

tileMap :: [String] -> [Tile2]
tileMap = concatMap (\(a, c) -> map (\(b,e) -> ((a,b), e)) $ tileEdges c) . tileAngles

upsert :: a -> Maybe [a] -> Maybe [a]
upsert a = (<|> pure [a]) . fmap (a :)

edgeMap :: [(Int, [Tile2])] -> Map String [EdgeTile]
edgeMap =
    foldl (\acc (tile, edges) -> foldl (yoo tile) acc edges) empty where
    yoo tile acc ((c,a), edge) = alter (upsert (tile,(c,a))) edge acc

-- Finding the topleft corner tile

isOuterEdge :: [EdgeTile] -> Bool
isOuterEdge = (== 1) .length . nub . map fst

cornerTopEdge :: [(String, Int)] -> (Int, String)
cornerTopEdge =
    second head . head . toList . M.filter ((== 4) . length) . foldl xx empty where
    xx acc (edge, tile) = alter (upsert edge) tile acc

topCorner :: Map String [EdgeTile] -> [(Int, [Tile2])] -> Maybe (Int, Int)
topCorner edges tiles =
    let (tile, edge) = cornerTopEdge . toList . fmap (fst . head)  $ M.filter isOuterEdge edges
    in (tile,) . fst . fst . head . filter (liftA2 (&&) ((== 0) . snd . fst) ((== edge) . snd)) <$> lookup tile tiles

-- Solving the puzzle

unconsFst :: [a] -> Maybe a
unconsFst = fmap fst . uncons

angledTileEdge :: EdgeTile -> [(Int, [Tile2])] -> Maybe String
angledTileEdge (i, (a, e)) tiles = do
    fmap snd $ (>>= unconsFst) $ filter ((== (a,e)) . fst) <$> lookup i tiles

angledEdgeTile :: Map String [EdgeTile] -> Int -> String -> Maybe [EdgeTile]
angledEdgeTile edges e edge =
    filter ((== e) . snd . snd) <$> M.lookup edge edges

bottomNeighbor :: LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> Maybe LocatedTile
bottomNeighbor (i, a) edges =
    fmap (\(t, (a,_)) -> (t,a)) . ((unconsFst . filter ((/= i) . fst)) <=< angledEdgeTile edges 0) <=< angledTileEdge (i, (a, 3))

rightNeighbor :: LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> Maybe LocatedTile
rightNeighbor (i, a) edges =
    fmap (\(t, (a,_)) -> (t,a)) . (( unconsFst . filter ((/= i) . fst)) <=< angledEdgeTile edges 1) <=< angledTileEdge (i, (a, 2))

leftNeighbor :: LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> Maybe LocatedTile
leftNeighbor (i, a) edges =
    fmap (\(t, (a,_)) -> (t,a)) . ((unconsFst . filter ((/= i) . fst)) <=< angledEdgeTile edges 2) <=< angledTileEdge (i, (a, 1))

solveDown :: Maybe LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> [LocatedTile]
solveDown (Just t) edges tiles = t : solveDown (bottomNeighbor t edges tiles) edges tiles
solveDown Nothing _ _ = []

solveRight :: Maybe LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> [LocatedTile]
solveRight (Just t) edges tiles = t : solveRight (rightNeighbor t edges tiles) edges tiles
solveRight Nothing _ _ = []

solveLeft :: Maybe LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> [LocatedTile]
solveLeft (Just t) edges tiles = t:  solveLeft (leftNeighbor t edges tiles) edges tiles
solveLeft Nothing _ _ = []

solveHorizontal :: LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> [LocatedTile]
solveHorizontal t edges tiles =
    case solveRight (pure t) edges tiles of
         [_] -> reverse $ solveLeft (pure t) edges tiles
         x -> x

solvePuzzle :: Maybe LocatedTile -> Map String [EdgeTile] -> [(Int, [Tile2])] -> [[LocatedTile]]
solvePuzzle corner edges tiles =
    map (\a -> solveHorizontal a edges tiles) $ solveDown corner edges tiles

-- Formatting the puzzle [[Located Tile]] -> [String]

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

trimTile :: [String] -> [String]
trimTile = map (drop 1 . init) . drop 1 . init

fmtPuzzle :: [[String]] -> [String]
fmtPuzzle p =
    let len = round $ sqrt $ fromIntegral $ length p
    in concatMap (map (intercalate "") . transpose . map trimTile) $ partition len p

buildPuzzle :: [(Int, [String])] -> [[LocatedTile]] -> [String]
buildPuzzle strTiles =
    fmtPuzzle . catMaybes . join . map (map (join . (\(id, a) -> fmap (lookup a . tileAngles) (lookup id strTiles))))

-- Monster lookup in solved [String] puzzle

sumTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuples (x,y) (a,b) = (x+a, y+b)

monsterCoords = [ (18, 0)
                , (0, 1), (5, 1), (6, 1), (11, 1), (12, 1), (17, 1), (18, 1), (19, 1)
                , (1, 2), (4, 2), (7, 2), (10, 2), (13, 2), (16, 2)
                ]

isMonsterPart :: (Int, Int) -> [String] -> Bool
isMonsterPart (x,y) = (== '#') . (!! x) . (!! y)

hasMonster :: (Int, Int) -> [String] -> Bool
hasMonster offset s = all ((`isMonsterPart` s) . sumTuples offset) monsterCoords

canFitMonsterY :: Int -> [String] -> Bool
canFitMonsterY y = ((y + 3) <=) . length

canFitMonsterX :: Int -> [String] -> Bool
canFitMonsterX x =  ((x + 19) <=) . length . head

canFitMonster :: (Int, Int) -> [String] -> Bool
canFitMonster (x,y) = liftA2 (&&) (canFitMonsterX x) (canFitMonsterY y)

countMonsters :: [String] -> Int
countMonsters s =
    go (0,0) where
    go c
        | canFitMonster c s = fromEnum (hasMonster c s) + go (first (+ 1) c)
        | canFitMonsterY (snd c) s = go $ bimap (const 0) (+ 1) c
        | otherwise = 0

--

countPuzzleMonsters = maximum . map (countMonsters . snd) . tileAngles

waterRoughness :: [String] -> Int
waterRoughness s = length ( filter (== '#') $ unlines s) - (15 * countPuzzleMonsters s)

cornerProduct :: [LocatedTile] -> Int
cornerProduct = liftA2 (*) (fst . head) (fst . last)

main = do
    strTiles <- parsedInput (2020, 20) (map parseTile . groupedLines . lines)
    let tiles = map (second tileMap) strTiles
    let edges = edgeMap tiles
    let corner = topCorner edges tiles
    let solved = solvePuzzle corner edges tiles
    print $ liftA2 (*) (cornerProduct . head) (cornerProduct . last) solved
    print $ waterRoughness $ buildPuzzle strTiles solved
