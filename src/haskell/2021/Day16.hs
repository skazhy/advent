{-|
Module      : Day16
Description : Day 16: Packet Decoder

<https://adventofcode.com/2021/day/16>
-}

module Day16 where

import Advent
import Data.Bifunctor (bimap, first)
import Data.Char (digitToInt, intToDigit)
import Data.List (tails)
import Numeric (readHex, showIntAtBase)

data Packet =  Literal Int Int | Op ([Int] -> Int) Int [Packet]

compareTails :: (Int -> Int -> Bool) -> [Int] -> Int
compareTails op s = fromEnum $ and [ op a b | (a:b:_) <- tails s]

fromString :: String -> ([Int] -> Int)
fromString "000" = sum
fromString "001" = product
fromString "010" = minimum
fromString "011" = maximum
fromString "101" = compareTails (>)
fromString "110" = compareTails (<)
fromString "111" = compareTails (==)

runOp :: Packet -> Int
runOp (Literal _ v) = v
runOp (Op opFn _ sub) = opFn $ map runOp sub

versionSum :: Packet -> Int
versionSum (Literal v _) = v
versionSum (Op _ v sub) = foldl (\acc s -> acc + versionSum s) v sub

toPaddedBinary :: Int -> [Char]
toPaddedBinary c =
    concat (replicate (4 - length b) "0") ++ b where
    b = showIntAtBase 2 intToDigit c ""

hexToPaddedBinary :: String -> String
hexToPaddedBinary = concatMap (toPaddedBinary . fst) . concatMap (readHex . (:[]))

toDecimal :: String -> Int
toDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0

decodeLiteral :: Int -> String -> [Packet]
decodeLiteral v =
    go [] where
    go res ('1':t) = go (res ++ take 4 t) (drop 4 t)
    go res (_:t) = Literal v (toDecimal $ res ++ take 4 t) : decodePacket (drop 4 t)

decodePacket :: String -> [Packet]
decodePacket s =
    case (take 3 $ drop 3 s, drop 6 s) of
        ("100", t) -> decodeLiteral v t
        (o, '1':t) -> uncurry (:) $ first (Op (fromString o) v) $ splitAt dec11 (decodePacket $ drop 11 t)
        (o, '0':t) -> uncurry (:) $ bimap (Op (fromString o) v . decodePacket) decodePacket $ splitAt dec15 $ drop 15 t
        (o, '0':t) -> Op (fromString o) v (decodePacket $ take dec15 $ drop 15 t) : decodePacket (drop (15 + dec15) t)
        _ -> []
    where dec15 = toDecimal $ take 15 $ drop 7 s
          dec11 = toDecimal $ take 11 $ drop 7 s
          v = toDecimal $ take 3 s

main = do
    input <- parsedInput (2021, 16) (head . decodePacket . hexToPaddedBinary)
    print $ versionSum input
    print $ runOp input
