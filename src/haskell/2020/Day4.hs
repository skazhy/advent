{-|
Module      : Day4
Description : Day 4: Passport Processing

<https://adventofcode.com/2020/day/4>
-}

module Day4 where

import Advent

import Control.Monad
import Data.List
import Data.Maybe (isJust)
import Data.Set (member, fromDistinctAscList)
import Numeric (readHex)
import Text.Read (readMaybe)

type Passport = [String]

readInt :: String -> Maybe Int
readInt i = readMaybe i :: Maybe Int

dropRight :: Int -> [a] -> [a]
dropRight i l = take (length l - i) l

-- Field presence

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredFields :: Passport -> Bool
hasRequiredFields = null . (requiredFields \\) . map (take 3)

-- Validation

validateStringIntInRange :: (Int, Int) -> String -> Bool
validateStringIntInRange r =
    maybe False (inRange r) . readInt

validateHeight :: String -> Bool
validateHeight h
    | isSuffixOf "cm" h = validateStringIntInRange (150, 193) (dropRight 2 h)
    | isSuffixOf "in" h = validateStringIntInRange (59, 76) (dropRight 2 h)
    | otherwise = False

validateHairColor :: String -> Bool
validateHairColor h
    | isPrefixOf "#" h && length h == 7 =
        -- Second element of readHex will contain unparsed data.
        ((== "") . snd . head . readHex . drop 1) h
    | otherwise = False

validEyeColors = fromDistinctAscList ["amb", "blu", "brn", "grn", "gry", "hzl", "oth"]

validatePassportId :: String -> Bool
validatePassportId p
    | length p == 9 = (isJust . readInt) p
    | otherwise = False

validateField :: String -> Bool
validateField s =
    case (take 3 s, drop 4 s) of
        ("byr", v) -> validateStringIntInRange (1920, 2002) v
        ("iyr", v) -> validateStringIntInRange (2010, 2020) v
        ("eyr", v) -> validateStringIntInRange (2020, 2030) v
        ("hgt", v) -> validateHeight v
        ("hcl", v) -> validateHairColor v
        ("ecl", v) -> member v validEyeColors
        ("pid", v) -> validatePassportId v
        _ -> True

main = do
    input <- parsedInput (2020, 4) (map (concatMap words) . groupedLines . lines)
    print $ (length . filter hasRequiredFields) input
    print $ (length . filter (liftM2 (&&) hasRequiredFields (all validateField))) input
