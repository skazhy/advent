{-|
Module      : Day20
Description : Day 20: Trench Map

<https://adventofcode.com/2021/day/20>
-}

module Day20 where

import Debug.Trace

import Advent
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)

replaceChar :: Char -> Char
replaceChar '#' = '1'
replaceChar '.' = '0'

toDecimal :: String -> Int
toDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0 . map replaceChar

get :: Int -> [a] -> Maybe a
get n xs | n < 0 = Nothing
         | n >= length xs = Nothing
         | otherwise = Just $ xs !! n

padImage :: Char -> [String] -> [String]
padImage blank img =
  map (\a -> blank : a ++ [blank]) $ padRow : img ++ [padRow]
  where padRow = replicate (length $ head img) blank

enhancePixel :: Char -> String -> [String] -> (Int, Int) -> Char
enhancePixel c algo img (x,y) =
  algo !! toDecimal [fromMaybe c (get x img >>= get y) | x <- [x-1..x+1] , y <- [y-1..y+1]]

enhanceImg :: String -> [String] -> Char -> [String]
enhanceImg algo img blank =
  reverse $ foldl (\acc (x, row) -> enhanceRow x row : acc) [] $ zip [0..] paddedImg
  where paddedImg = padImage blank img
        enhanceRow x row = zipWith (\y c -> enhancePixel c algo paddedImg (x,y)) [0..] row

runEnhance :: Int -> String -> [String] -> [String]
runEnhance i algo s =
  foldl (enhanceImg algo) s $ take i (cycle ['.', head algo])

pixelCount :: [String] -> Int
pixelCount = length . filter (== '#') . concat

main = do
    (algo, img) <- parsedInput (2021, 20) (bimap head tail . break (== "") . lines)
    print $ pixelCount $ runEnhance 2 algo img
    print $ pixelCount $ runEnhance 50 algo img
