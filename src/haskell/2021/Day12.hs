{-|
Module      : Day12
Description : Day 12: Passage Pathing

<https://adventofcode.com/2021/day/12>
-}

module Day12 where

import Advent
import Data.Bifunctor (bimap)
import Data.Char (isLower)
import Data.List (elemIndex, find ,group, sort)
import Data.Maybe (mapMaybe, isJust)
import Data.Map (Map, alter, empty, filterWithKey, lookup, delete)

data Cave = Start | End | Big String | Small String deriving (Eq, Ord, Show)
type Graph = Map Cave [Cave]

mkCave :: String -> Cave
mkCave "start" = Start
mkCave "end" = End
mkCave c@(x:_) | isLower x = Small c
               | otherwise = Big c

splitString :: String -> Maybe (Cave,Cave)
splitString s = fmap (bimap mkCave (mkCave . tail) . (`splitAt` s)) (elemIndex '-' s)

updateNode :: Cave -> Maybe [Cave] -> Maybe [Cave]
updateNode s Nothing = Just [s]
updateNode s v = fmap (s:) v

updateNodes :: Graph -> (Cave, Cave) -> Graph
updateNodes graph (Start,b) = alter (updateNode b) Start graph
updateNodes graph (a,Start) = alter (updateNode a) Start graph
updateNodes graph (a,b) = alter (updateNode a) b $ alter (updateNode b) a graph

makeGraph :: [String] -> Graph
makeGraph = foldl updateNodes empty . mapMaybe splitString

paths :: Graph -> [[Cave]]
paths graph =
  go graph [] Start where
  go graph path root =
    case (root, Data.Map.lookup root graph) of
      (End, _) -> [End:path]
      (_, Nothing) -> []
      (Small _, Just adjacent) -> concatMap (go (delete root graph) (root:path)) adjacent
      (_, Just adjacent) -> concatMap (go graph (root:path)) adjacent

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

paths2 :: Graph -> [[Cave]]
paths2 graph =
  go graph [] False Start where
    go graph path visitedSmall root =
      case (root, Data.Map.lookup root graph) of
        (End, _) -> [End:path]
        (_, Nothing) -> []
        (Small _, Just adjacent) ->
          if visitedSmall
          then concatMap (go (delete root graph) (root:path) True) adjacent
          else if isJust $ find (== root) path
          then concatMap (go (foldl (flip delete) graph (filter isSmall path)) (root:path) True) adjacent
          else concatMap (go graph (root:path) False) adjacent
        (_, Just adjacent) -> concatMap (go graph (root:path) visitedSmall) adjacent

main = do
    input <- parsedInput (2021, 12) (makeGraph . lines)
    print $ length $ paths input
    print $ length $ paths2 input
