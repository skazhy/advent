{-|
Module      : Day8
Description : Day 8: Resonant Collinearity

<https://adventofcode.com/2024/day/8>
-}

module Day8 where

import Advent
import Data.Bifunctor (bimap)
import Data.Map (Map, empty, alter)
import Data.List (sortOn, nub, tails)

pairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairs = go . sortOn fst where
  go [] = []
  go (h:t) = map (h,) t ++ pairs t

antinodeSeq :: (Int -> Int) -> (Int -> Int) -> (Int, Int) -> [(Int, Int)]
antinodeSeq fy fx p = p : antinodeSeq fy fx (bimap fy fx p)

upd :: Char ->  (Int, Int) -> Maybe [(Int, Int)] -> Maybe [(Int, Int)]
upd '.' _ _ = Nothing
upd _ p Nothing = Just [p]
upd _ p (Just v) = Just $ p : v

antennaPoints :: [String] -> Map Char [(Int, Int)]
antennaPoints =
    foldl
        (\acc (y, row) ->
            foldl
                (\acc (x, c) -> alter (upd c (y, x)) c acc)
                acc
                (zip [0..] row)
        )
        empty
        . zip [0..]

antinodes :: (Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
antinodes (y1, x1) (y2, x2)
  | x1 < x2 =
      ( antinodeSeq (subtract dy) (subtract dx) (y1, x1),
        antinodeSeq (+ dy) (+ dx) (y2, x2)
      )
  | otherwise =
      ( antinodeSeq (subtract dy) (+ dx) (y1, x1),
        antinodeSeq (+ dy) (subtract dx) (y2, x2)
      )
  where
    dy = y2 - y1
    dx = abs $ x1 - x2

antennaTypeAntinodes :: ([(Int, Int)] -> [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)]
antennaTypeAntinodes f points =
  concatMap (uncurry (++) . bimap f f . uncurry antinodes) $ pairs points

antinodeCount :: ([(Int, Int)] -> [(Int, Int)]) -> [String] -> Int
antinodeCount f r =
  length $ nub $ foldl (\acc r -> acc ++ antennaTypeAntinodes (f . takeWhile inBounds) r) [] $ antennaPoints r
  where
    inBounds (y, x) = 0 <= y && 0 <= x && y < length r && x < length (head r)

main = do
    input <- parsedInput (2024, 8) lines
    print $ antinodeCount (take 1 . drop 1) input
    print $ antinodeCount id input
