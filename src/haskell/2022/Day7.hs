{-|
Module      : Day7
Description : Day 7: No Space Left On Device

<https://adventofcode.com/2022/day/7>
-}

module Day7 where

import Advent
import Data.Map (Map, adjust, fromList, insert, keys, elems, (!))
import Data.List (sort)
import Data.Bifunctor (first, second)

type Directory = (Int, [String])
type FileSystem = ([String], Map String Directory)

emptyDirectory :: Directory
emptyDirectory = (0, [])

emptyFileSystem :: FileSystem
emptyFileSystem = (["/"], fromList [("//", emptyDirectory)])

parseStdOut :: FileSystem -> [String] -> FileSystem
parseStdOut (_:prev, fs) ("$":"cd":"..":_) = (prev,fs)
parseStdOut (curr:prev, fs) ("$":"cd":f:_) = ((curr++f):curr:prev,fs)
parseStdOut fs ("$":"ls":_) = fs
parseStdOut (curr:prev, fs) ("dir":name:_) =
    (curr:prev, adjust (second ((curr ++ name) :)) curr $ insert (curr ++ name) emptyDirectory fs)
parseStdOut (curr:prev, fs) (size:name:_) =
    (curr:prev, adjust (first (+ read size)) curr fs)

directorySize :: Map String Directory -> String -> Int
directorySize fs dirname =
    case fs ! dirname of
        (size, []) -> size
        (size, children) ->  size + sum (map (directorySize fs) children)

parse :: [String] -> Map String Int
parse input =
    fromList $ map (\x -> (x, directorySize fs x)) (keys fs)
    where
        fs = snd $ foldl parseStdOut emptyFileSystem $ map words input

--- Part 2: Size of smallest directory that needs to be deleted

part2 :: Map String Int -> Int
part2 parsed =
    head $ dropWhile (< sizeRequired) $ sort $ elems parsed
    where
          sizeRequired = subtract 40000000 $ parsed ! "//"

main = do
    input <- parsedInput (2022, 7) (parse . lines)
    print $ (sum . filter (<= 100000) . elems) input
    print $ part2 input
