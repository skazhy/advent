module Data.Int (ceilDiv, readBinaryInt) where

import Data.Char (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric (readInt)

infixl 7 `ceilDiv`

ceilDiv :: Int -> Int -> Int
ceilDiv a b =
    case a `divMod` b of
        (r, 0) -> r
        (r, _) -> r + 1

readBinaryInt :: String -> Maybe Int
readBinaryInt = fmap fst . listToMaybe . readInt 2 (const True) digitToInt
