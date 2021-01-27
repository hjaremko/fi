module Parse.Expression where

import Grammar.Grammar
import Parse.Primitive
import Parse.Numbers
import Parse.Identifier

-- expr :: Parser Expr
-- expr = int `bind` \l ->
--         char '+' `bind` \op ->
--        int `bind` \r ->
--         result (Add l r)

expression :: Parser Expr
expression =
  first float
    -- expression = expr
    -- `plus` float
    `plus` first int
    `plus` first (arithm `bind` \a -> result (Arithm a))


-- `plus` result (FloatLiteral 0)

-- arithm :: Parser Expr
-- arithm = many arithmChar `bind` \a ->
--         result (Arithm a)


-- arithmOp :: Parser Char 
-- arithmOp = char '+'
--           `plus` char '-'
--           `plus` char '/'
--           `plus` char '*'

-- space :: Parser Char 
-- space = consumeIf ( == ' ')

-- paren :: Parser Char 
-- paren = consumeIf ( == '(') `plus` consumeIf (== ')')

-- dot :: Parser Char
-- dot = consumeIf (== '.')

-- arithmChar :: Parser Char 
-- arithmChar = space `plus` arithmOp `plus` paren `plus` alphanum `plus` dot
----------------------------------------------------------------------------------------------
arithm :: Parser [Token]
arithm = many (constSymbol 
  `plus` leftParen
  `plus` rightParen
  `plus` ops
  `plus` vari
  )


-- arithmOp :: Parser Char 
-- arithmOp = char '+'
--           `plus` char '-'
--           `plus` char '/'
--           `plus` char '*'

vari :: Parser Token
vari = identificator `bind` \i ->
      result (Variable i)

leftParen :: Parser Token
leftParen = consumeIf ( == '(') `bind` \p ->
            result LeftParen

rightParen :: Parser Token
rightParen = consumeIf ( == ')') `bind` \p ->
            result RightParen

ops :: Parser Token
ops = plusOp `plus` minusOp `plus` multOp `plus` divOp

plusOp :: Parser Token
plusOp = consumeIf ( == '+') `bind` \p ->
          result Plus

minusOp :: Parser Token
minusOp = consumeIf ( == '-') `bind` \p ->
            result Minus

multOp :: Parser Token
multOp = consumeIf ( == '*') `bind` \p ->
            result Mult

divOp :: Parser Token
divOp = consumeIf ( == '/') `bind` \p ->
            result Div

-- space :: Parser Char 
-- space = consumeIf ( == ' ')

-- paren :: Parser Char 
-- paren = consumeIf ( == '(') `plus` consumeIf (== ')')

-- dot :: Parser Char
-- dot = consumeIf (== '.')

-- arithmChar :: Parser Char 
-- arithmChar = space `plus` arithmOp `plus` paren `plus` alphanum `plus` dot

constSymbol :: Parser Token
constSymbol = first (float' `plus` naturalF) 
      `bind` \ v ->
        result (Const v)

-- data Token
--   = Const Float
--   | Plus
--   | Minus
--   | Mult
--   | Div
--   | Sqrt
--   | Variable String
--   | LeftParen
--   | RightParen
--   deriving (Show)

-- -- data Op
-- --   = LeftParen Int
-- --   | Plus Int
-- priority :: Token -> Int
-- priority LeftParen = 0
-- priority Plus = 1
-- priority Minus = 1
-- priority RightParen = 1
-- priority Mult = 2
-- priority Div = 2

-- toRpn :: [Token] -> [Token] -> [Token] -> [Token]
-- toRpn [] out stack = out ++ stack
-- toRpn (Const v:xs) out stack = toRpn xs (out ++ [Const v]) stack
-- toRpn (Variable i:xs) out stack = toRpn xs (out ++ [Variable i]) stack
-- toRpn (LeftParen:xs) out stack =  toRpn xs out (LeftParen:stack)
-- toRpn (RightParen:xs) out stack =
--   let os = popUntilLeftParen out stack
--   in uncurry (toRpn xs) os

-- toRpn (op:ops) out [] = 
--     toRpn ops out [op]

-- toRpn (op:ops) out (s:stack) = 
--   if priority op > priority s then
--     toRpn ops out (op:s:stack)
--   else 
--     let os = popHigherOrEqualPriorityOps out (s:stack) op
--     in uncurry (toRpn ops) os



-- popHigherOrEqualPriorityOps :: [Token] -> [Token] -> Token -> ([Token], [Token])
-- popHigherOrEqualPriorityOps out [] t = (out, [t])
-- popHigherOrEqualPriorityOps out (op:stack) t =
--   if priority op >= priority t then
--     popHigherOrEqualPriorityOps (out ++ [op]) stack t
--   else (out, t:op:stack)

-- popUntilLeftParen :: [Token] -> [Token] -> ([Token], [Token])
-- popUntilLeftParen out (LeftParen:stack) = (out, stack)
-- popUntilLeftParen out (s:stack) = popUntilLeftParen (out ++ [s]) stack


-- -- [Const 12.0,Plus,Variable "a",Mult,LeftParen,Variable "b",Mult,Variable "c",Plus,Variable "d",Div,Variable "e",RightParen]

-- -- [] [Mult, Mult, Mult, Mult, Mult, LeftParen, Mult]
-- -- [] [Mult, LeftParen, Mult, Plus]

-- -- [Const 2.0,Const 7.0,Plus,Const 3.0,Div,Const 14.0,Const 3.0,Minus,Const 4.0,Mult,Plus,Const 2.0,Div]

-- evalRpn :: [Token] -> [Float] -> Float
-- evalRpn [] [s] = s
-- evalRpn (Const v:ops) stack = evalRpn ops (v:stack)
-- evalRpn (Variable v:ops) stack = evalRpn ops (0:stack)
-- evalRpn (Plus:ops) (a:b:stack) = evalRpn ops ((b + a):stack)
-- evalRpn (Minus:ops) (a:b:stack) = evalRpn ops ((b - a):stack)
-- evalRpn (Mult:ops) (a:b:stack) = evalRpn ops ((b * a):stack)
-- evalRpn (Div:ops) (a:b:stack) = evalRpn ops ((b / a):stack)
