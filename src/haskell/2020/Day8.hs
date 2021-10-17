{-|
Module      : Day8
Description : Day 8: Handheld Halting

<https://adventofcode.com/2020/day/8>
-}

module Day8 where

import Advent

import Data.List (isPrefixOf)
import Data.Set (Set, empty, member, insert)

data Instruction = Nop Int | Acc Int | Jmp Int | Halt Int | LoopHalt Int

instance Show Instruction where
  show (LoopHalt a) = show a

isHaltInstruction :: Instruction -> Bool
isHaltInstruction (Halt _) = True
isHaltInstruction (LoopHalt _) = True
isHaltInstruction _ = False

swapInstruction :: Instruction -> Maybe Instruction
swapInstruction (Nop a) = Just $ Jmp a
swapInstruction (Jmp a) = Just $ Nop a
swapInstruction _ = Nothing

-- Parsing

readNumber :: String -> Int
readNumber s
    | "+" `isPrefixOf` s = (read . tail) s
    | otherwise = read s

fromString :: String -> Instruction
fromString s =
    let ws = words s
        in case head ws of
                "nop" -> Nop (readNumber $ ws !! 1)
                "acc" -> Acc (readNumber $ ws !! 1)
                "jmp" -> Jmp (readNumber $ ws !! 1)

-- Running

data State =
    State { position :: Int
          , accumulator :: Int
    } deriving (Show)

newState = State { position = 0, accumulator = 0 }

getInstruction :: [Instruction] -> State -> Set Int -> Instruction
getInstruction instructions state seen
    | length instructions <= position state = Halt (accumulator state)
    | member (position state) seen = LoopHalt (accumulator state)
    | otherwise = instructions !! position state

runInstruction :: Instruction -> State -> State
runInstruction (Nop _) state@State{position=p} = state { position = p + 1 }
runInstruction (Jmp i) state@State{position=p} = state { position = p + i }
runInstruction (Acc i) State{position=p, accumulator=acc} =
    State { position = p + 1, accumulator = acc + i }

runInstructions :: State -> Set Int -> [Instruction] -> Instruction
runInstructions state seen instructions =
    case getInstruction instructions state seen of
         ins | isHaltInstruction ins -> ins
         ins -> runInstructions (runInstruction ins state) (insert (position state) seen) instructions

runProgram = runInstructions newState empty

runSwapped :: [Instruction] -> [Instruction] -> Int
runSwapped (x:xs) inits =
    case runProgram . (inits ++) . (: xs) <$> swapInstruction x of
         Just (Halt a) -> a
         _ -> runSwapped xs $ inits ++ [x]

main = do
    input <- parsedInput (2020, 8) (map fromString . lines)
    print $ runProgram input
    print $ runSwapped input []
