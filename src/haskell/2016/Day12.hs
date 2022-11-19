{-|
Module      : Day12
Description : Day 12: Leonardo's Monorail

<https://adventofcode.com/2016/day/12>
-}

module Day12 where

import Advent

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Read (readMaybe)

data Cmd
  = Inc String
  | Dec String
  | Cpy String String
  | Jnz String Int deriving (Show)

parseRow :: String -> Cmd
parseRow s =
  let (cmd:args) = words s
  in case cmd of
    "inc" -> Inc $ head args
    "dec" -> Dec $ head args
    "cpy" -> Cpy (head args) (last args)
    _ -> Jnz (head args) (read $ last args)

parseRows :: [String] -> Map Int Cmd
parseRows = Map.fromList . zip [0..] . map parseRow

initialRegisters :: Int -> Map String Int
initialRegisters c = Map.fromList [("a", 0), ("b", 0), ("c", c), ("d", 0)]

getValue :: Map String Int -> String -> Int
getValue registers v =
    case (readMaybe v :: Maybe Int) of
      Just i -> i
      Nothing -> registers ! v

applyCmd :: (Map String Int, Int) -> Cmd -> (Map String Int, Int)
applyCmd (acc, ptr) (Inc a) = (Map.adjust (+1) a acc, ptr + 1)
applyCmd (acc, ptr) (Dec a) = (Map.adjust (subtract 1) a acc, ptr + 1)
applyCmd (acc, ptr) (Cpy from to) =
  (Map.insert to (getValue acc from) acc, ptr + 1)
applyCmd (acc, ptr) (Jnz pred jmp)
  | getValue acc pred == 0 = (acc, ptr + 1)
  | otherwise = (acc, ptr + jmp)

runCmds :: Map Int Cmd -> Int -> Int
runCmds cmds c =
  go 0 (initialRegisters c)
  where
    go idx registers
      | Map.member idx cmds =
          let
            (newReg, newIdx) = applyCmd (registers, idx) (cmds ! idx)
          in
            go newIdx newReg
      | otherwise = registers ! "a"

main = do
    input <- parsedInput (2016, 12) (parseRows . lines)
    print $ runCmds input 0
    print $ runCmds input 1
