{-|
Module      : Day14
Description : Day 14: Extended Polymerization

<https://adventofcode.com/2021/day/14>
-}

module Day14 where

import Advent
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.List (tails)
import Data.Map (Map, adjust, alter, elems, empty, fromList, foldlWithKey, fromListWith, toList, (!))
import Data.Maybe (mapMaybe)

loadInput = parsedInput (2021, 14) lines

type Frequencies = Map String Int
type Rules = Map String [String]

parseRule :: String -> Maybe (String, [String])
parseRule (a:b:_:_:_:_:c:_) = Just ([a,b], [[a,c], [c,b]])
parseRule _ = Nothing

initFrequencies :: String -> Frequencies
initFrequencies s =
    fromListWith (+) $ zip (takeWhile ((== 2) . length) $ map (take 2) $ tails s) (repeat 1)

alterFrequency :: Int -> Maybe Int -> Maybe Int
alterFrequency a f = fmap (+a) f <|> pure a

growPolymerStep :: Rules -> Frequencies -> Frequencies
growPolymerStep rules =
    foldlWithKey (\acc k v -> foldl (flip $ alter (alterFrequency v)) acc (rules ! k)) empty

growPolymer :: Int -> Rules -> Frequencies -> Frequencies
growPolymer n rules  = (!! n) . iterate (growPolymerStep rules)

charFrequencies :: String -> Frequencies -> [Int]
charFrequencies template =
    elems . adjust (+1) (last template) . fromListWith (+) . map (first head) . toList

getResult :: String -> Frequencies -> Int
getResult template =
    foldl1 subtract . sequence [minimum, maximum] . charFrequencies template

main = do
    (template:_:rules) <- loadInput
    print $ getResult template $ growPolymer 10 (fromList $ mapMaybe parseRule rules) (initFrequencies template)
    print $ getResult template $ growPolymer 40 (fromList $ mapMaybe parseRule rules) (initFrequencies template)
