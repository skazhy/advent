{-# LANGUAGE OverloadedStrings #-}

module Advent
    ( readInput
    , parsedInput
    , intLines
    , intSequence
    , inRange
    , groupedLines
    , printMaybe
    , foldGrid
    ) where

import Control.Monad
import qualified Data.Text as T

inputFilename :: (Int, Int) -> String
inputFilename (yearNum,dayNum) =
    "resources/" ++ show yearNum ++ "/day" ++ show dayNum ++ ".txt"

readInput :: (Int, Int) -> IO String
readInput = fmap init . readFile . inputFilename

parsedInput :: (Int, Int) -> (String -> a) -> IO a
parsedInput yearDay parse =
    parse <$> readInput yearDay

-- Input parsers

intLines :: String -> [Int]
intLines = map read . lines

intSequence :: String -> [Int]
intSequence = map (read . T.unpack) . T.splitOn "," . T.pack

-- Helpers

inRange :: Ord a => (a, a) -> a -> Bool
inRange (low, high) = liftM2 (&&) (>= low) (<= high)

-- |Groups lines in blocks that are separated by a blank line.
groupedLines :: [String] -> [[String]]
groupedLines [] = []
groupedLines (x:xs) =
    (x:ys) : (groupedLines . drop 1) zs
    where (ys,zs) = span (/= "") xs

printMaybe :: Show a => Maybe a -> IO ()
printMaybe (Just x) = print x
printMaybe Nothing = return ()

foldGrid :: (b -> Char -> (Int, Int) -> b) -> b -> [String] -> b
foldGrid f acc =
    foldl
        (\acc (y, row) ->
            foldl (\acc (x, c) -> f acc c (y, x))
                acc
                (zip [0..] row)
        )
        acc
        . zip [0..]
