-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Eval.Rpn where

import Eval.Context
import Grammar.Expression

priority :: Token -> Int
priority LeftParen = 0
priority Equals = 8
priority NotEquals = 8
priority Less = 8
priority LessEquals = 8
priority Greater = 8
priority GreaterEquals = 8
priority Plus = 12
priority Minus = 12
priority RightParen = 12
priority Mult = 13
priority Div = 13
priority Sqrt = 14
priority UnaryMinus = 15

toRpn :: [Token] -> [Token] -> [Token] -> [Token]
toRpn [] out stack = out ++ stack
toRpn (Const v : xs) out stack = toRpn xs (out ++ [Const v]) stack
toRpn (Variable i : xs) out stack = toRpn xs (out ++ [Variable i]) stack
toRpn (LeftParen : xs) out stack = toRpn xs out (LeftParen : stack)
toRpn (RightParen : xs) out stack =
  let os = popUntilLeftParen out stack
   in uncurry (toRpn xs) os
toRpn (op : ops) out [] = toRpn ops out [op]
toRpn (op : ops) out (s : stack) =
  if priority op > priority s
    then toRpn ops out (op : s : stack)
    else
      let os = popHigherOrEqualPriorityOps out (s : stack) op
       in uncurry (toRpn ops) os

popHigherOrEqualPriorityOps :: [Token] -> [Token] -> Token -> ([Token], [Token])
popHigherOrEqualPriorityOps out [] t = (out, [t])
popHigherOrEqualPriorityOps out (op : stack) t =
  if priority op >= priority t
    then popHigherOrEqualPriorityOps (out ++ [op]) stack t
    else (out, t : op : stack)

popUntilLeftParen :: [Token] -> [Token] -> ([Token], [Token])
popUntilLeftParen out (LeftParen : stack) = (out, stack)
popUntilLeftParen out (s : stack) = popUntilLeftParen (out ++ [s]) stack

evalRpn :: [Token] -> [Float] -> State -> Float
evalRpn [] [s] _ = s
evalRpn (Const v : ops) stack state = evalRpn ops (v : stack) state
evalRpn (Variable v : ops) stack state = evalRpn ops (varValue v state : stack) state
evalRpn (UnaryMinus : ops) (a : stack) state = evalRpn ops (- a : stack) state
evalRpn (Plus : ops) (a : b : stack) state = evalRpn ops ((b + a) : stack) state
evalRpn (Minus : ops) (a : b : stack) state = evalRpn ops ((b - a) : stack) state
evalRpn (Mult : ops) (a : b : stack) state = evalRpn ops ((b * a) : stack) state
evalRpn (Div : ops) (a : b : stack) state = evalRpn ops ((b / a) : stack) state
evalRpn (Sqrt : ops) (a : stack) state = evalRpn ops (sqrt a : stack) state
evalRpn (Equals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b == a) : stack) state
evalRpn (NotEquals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b /= a) : stack) state
evalRpn (Less : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b < a) : stack) state
evalRpn (LessEquals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b <= a) : stack) state
evalRpn (Greater : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b > a) : stack) state
evalRpn (GreaterEquals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b >= a) : stack) state

boolToFloat :: Bool -> Float
boolToFloat op = if op then 1.0 else 0.0
