{-|
Module      : Day4
Description : Day 4: Ceres Search

<https://adventofcode.com/2024/day/4>
-}

module Day4 where

import Advent

-- Returns (y,x) values of all characters matching the predicate in the grid.
startCoords :: (Char -> Bool) -> [String] -> [(Int, Int)]
startCoords pred =
  concatMap (\(y, r) -> map ((y,) . fst) $ filter (pred . snd) $ zip [0..] r) . zip [0..]

charAtIdx :: [String] -> (Int, Int) -> Maybe Char
charAtIdx g (y, x) | y < 0 || x < 0 = Nothing
                   | y < length g &&  x < length row = Just $ (g !! y) !! x
                   | otherwise = Nothing
                   where row = g !! y

directions :: [(Int, Int)]
directions = [ (0,1), (0,-1)
             , (1,0), (-1,0)
             , (-1,1), (1,-1)
             , (1,1), (-1,-1)]

hasWord :: [String] -> String -> (Int, Int) -> (Int, Int) -> Bool
hasWord grid wordTail (y, x) (dy, dx) =
  go wordTail (y, x) where
  go [] _ = True
  go (w:ws) (y, x) =
    case charAtIdx grid (y, x) of
        Just c | c == w -> go ws (y + dy, x + dx)
            | otherwise -> False
        Nothing -> False

-- Returns true if given (y,x) coordinate is topleft corner of the MAS cross.
hasCross :: [String] -> (Int, Int) -> Bool
hasCross grid (y, x) =
  (hasWord grid "SAM" (y,x) (1,1) || hasWord grid "MAS" (y, x) (1,1)) && (hasWord grid "SAM" (y + 2,x) (-1,1) || hasWord grid "MAS" (y + 2,x) (-1,1))

-- Returns count of XMASes that originate from given (y, x) coord
xmasCount :: [String] -> (Int, Int) -> Int
xmasCount grid coords = length $ filter (hasWord grid "XMAS" coords) directions

main = do
    input <- parsedInput (2024, 4) lines
    print $ sum $ map (xmasCount input) $ startCoords (== 'X') input
    print $ length $ filter (hasCross input) $ startCoords (\c -> c == 'M' || c == 'S') input
