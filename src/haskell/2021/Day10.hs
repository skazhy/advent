{-|
Module      : Day10
Description : Day 10: Syntax Scoring

<https://adventofcode.com/2021/day/10>
-}

module Day10 where

import Advent
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Map (Map, fromList, lookup, member, (!))
import Data.Tuple (swap)

data Error = Corrupted Char | Incomplete String deriving (Show)

brackets = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

bracketMap = fromList brackets
inverseBracketMap = fromList $ map swap brackets

matchingBrackets :: Char -> Char -> Bool
matchingBrackets open close =
  (Just close ==) $ Data.Map.lookup open bracketMap

lineError :: String -> Maybe Error
lineError = go [] where
  go (x:xs) (y:ys)
    | matchingBrackets x y = go xs ys
    | Data.Map.member y inverseBracketMap = Just (Corrupted y)
    | otherwise = go (y:x:xs) ys
  go [] [] = Nothing
  go [] (x:xs)
    | Data.Map.member x bracketMap = go [x] xs
    | otherwise = Just (Incomplete xs)
  go x _ = Just (Incomplete x)

-- Scoring

corruptedScores = fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
incompleteScores = fromList [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

scoreError :: Error -> Int
scoreError (Corrupted a) = corruptedScores ! a
scoreError (Incomplete a) = foldl (\acc i -> (acc * 5) + i) 0 $ map (incompleteScores !) a

isCorrupted :: Error -> Bool
isCorrupted (Corrupted _) = True
isCorrupted _ = False

isIncomplete :: Error -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _ = False

--

middle :: [Int] -> Int
middle l = l !! (length l `div` 2)

main = do
    input <- parsedInput (2021, 10) (mapMaybe lineError . lines)
    print $ (sum . map scoreError . filter isCorrupted) input
    print $ (middle . sort . map scoreError . filter isIncomplete) input
