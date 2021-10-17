{-|
Module      : Day2
Description : Day 2: Password Philosophy

<https://adventofcode.com/2020/day/2>
-}

module Day2 where

import Advent

import Data.Bifunctor (bimap)

data Password =
    Password { letter :: Char
             , range :: (Int, Int)
             , value :: String
             } deriving (Show)

fromString :: String -> Password
fromString s =
    let [i, l, v] = words s
    in Password { range = bimap (abs . read) (abs . read) $ break (== '-') i
                , letter = head l
                , value = v
                }

isValidPassword :: Password -> Bool
isValidPassword pw =
    (inRange (range pw) . length . filter (== letter pw) . value) pw

charMatcher :: Password -> Int -> Bool
charMatcher pw val = value pw !! (val - 1) == letter pw

isValidPassword2 :: Password -> Bool
isValidPassword2 pw =
    uncurry (/=) $ bimap (charMatcher pw) (charMatcher pw) $ range pw

main = do
    input <- parsedInput (2020, 2) (map fromString . lines)
    print $ (length . filter isValidPassword) input
    print $ (length . filter isValidPassword2) input
