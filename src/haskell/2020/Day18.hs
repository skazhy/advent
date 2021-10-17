{-|
Module      : Day18
Description : Day 18: Operation Order

<https://adventofcode.com/2020/day/18>
-}

module Day18 where

import Advent

-- "High" precedence - evaluate immediately when receiving 2nd operand
-- "Low" precedence - evaluate after "high" precedence ops
data Precedence = Low | High
type Expression = (Maybe (Int -> Int), Maybe Int, Precedence)

emptyState = (Nothing, Nothing, High)

finalizeEval :: Expression -> Int
finalizeEval (Nothing,Just a,_) = a
finalizeEval (Just op, Just a,_) = op a

evalChar :: Bool -> [Expression] -> Char -> [Expression]
evalChar _ state ' ' = state
-- Operators
evalChar _ ((Nothing, Just a, _):xs) '+' = (Just (+ a), Nothing, High) :xs
evalChar _ ((Just op, Just a, p):xs) '+' = (Just (op . (+ a)), Nothing, p) :xs
evalChar False ((Nothing, Just a, _):xs) '*' = (Just (* a), Nothing, High) :xs
evalChar True ((Nothing, Just a, _):xs) '*' = (Just (* a), Nothing, Low) :xs
evalChar True ((Just op, Just a, _):xs) '*' = (Just (* op a), Nothing, Low) :xs
-- Brackets
evalChar _ state '(' = emptyState : state
evalChar True (h:(Just op,_,Low):xs) ')' = (Just op, Just (finalizeEval h), Low) : xs
evalChar _ (h:(Just op,_,p):xs) ')' = (Nothing, Just (op (finalizeEval h)), p) : xs
evalChar _ (h:(Nothing,_,p):xs) ')' = (Nothing, Just (finalizeEval h), p) : xs
-- Operands
evalChar _ ((Just op, _, High):xs) i = (Nothing, pure $ op $ read [i], High) :xs
evalChar _ ((op, _, p):xs) i = (op, pure $ read [i], p) :xs

runExpression :: Bool -> String -> Int
runExpression  hasPrecedence =
    finalizeEval . head . foldl (evalChar hasPrecedence) [emptyState]

main = do
    input <- parsedInput (2020, 18) lines
    print $ (sum . map (runExpression False)) input
    print $ (sum . map (runExpression True)) input
