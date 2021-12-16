{-|
Module      : Day16
Description : Day 16: Packet Decoder

<https://adventofcode.com/2021/day/16>
-}

module Day16 where

import Advent
import Data.Char (digitToInt, intToDigit)
import Data.List (tails)
import Numeric (readHex, showIntAtBase)

data Packet =  Literal Int Int | Op OpType Int [Packet] deriving (Show)

data OpType = Sum | Product | Min | Max | Gt | Lt | Eq deriving (Show)

fromString :: String -> OpType
fromString "000" = Sum
fromString "001" = Product
fromString "010" = Min
fromString "011" = Max
fromString "101" = Gt
fromString "110" = Lt
fromString "111" = Eq

compareTails :: (Int -> Int -> Bool) -> [Int] -> Int
compareTails op s = fromEnum $ all (\(a:b:_) -> op a b) $ filter ((== 2) . length) $ map (take 2) $ tails s

runOp :: Packet -> Int
runOp (Literal _ v) = v
runOp (Op Sum _ sub) = sum $ map runOp sub
runOp (Op Product _ sub) = product $ map runOp sub
runOp (Op Min _ sub) = minimum $ map runOp sub
runOp (Op Max _ sub) = maximum $ map runOp sub
runOp (Op Gt _ sub) = compareTails (>) $ map runOp sub
runOp (Op Lt _ sub) = compareTails (<) $ map runOp sub
runOp (Op Eq _ sub) = compareTails (==) $ map runOp sub

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
decodeLiteral v s =
    go [] s where
    go res s  =
        if head s == '1'
        then go (res ++ tail (take 5 s)) (drop 5 s)
        else Literal v (toDecimal $ res ++ tail (take 5 s)) : decodePacket (drop 5 s)

decodePacket :: String -> [Packet]
decodePacket s  =
    case (take 3 s, take 3 $ drop 3 s, drop 6 s) of
        (v, "100", t) -> decodeLiteral (toDecimal v) t
        (v, o, '1':t) -> let sub = (decodePacket $ drop 11 t)
                         in Op (fromString o) (toDecimal v) (take dec11 sub) : drop dec11 sub
        (v, o, '0':t) -> Op (fromString o) (toDecimal v) (decodePacket $ take dec15 $ drop 15 t) : decodePacket (drop (15 + dec15) t)
        _ -> []
    where dec15 = toDecimal $ take 15 $ drop 7 s
          dec11 = toDecimal $ take 11 $ drop 7 s

main = do
    input <- parsedInput (2021, 16) (head . decodePacket . hexToPaddedBinary)
    print $ versionSum input
    print $ runOp input
