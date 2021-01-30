-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Parse.Expression where

import Grammar.Expression
import Grammar.Statement
import Parse.Identifier
import Parse.Numbers
import Parse.Primitive

expression :: Parser Expr
expression = arithm `bind` \a -> result (Arithm (detectUnaryMinuses (removeSpaceToken a)))

removeSpaceToken :: [Token] -> [Token]
removeSpaceToken = filter (not . isSpaceToken)

detectUnaryMinuses :: [Token] -> [Token]
detectUnaryMinuses [] = []
detectUnaryMinuses [x] = [x]
detectUnaryMinuses (Minus : Const v : xs) = [UnaryMinus, Const v] ++ detectUnaryMinuses xs
detectUnaryMinuses (Minus : Variable v : xs) = [UnaryMinus, Variable v] ++ detectUnaryMinuses xs
detectUnaryMinuses (Const v : Minus : xs) = [Const v, Minus] ++ detectUnaryMinuses xs
detectUnaryMinuses (Variable v : Minus : xs) = [Variable v, Minus] ++ detectUnaryMinuses xs
detectUnaryMinuses (op1 : Minus : xs) = [op1, UnaryMinus] ++ detectUnaryMinuses xs
detectUnaryMinuses (lhs : op : xs) = [lhs, op] ++ detectUnaryMinuses xs

isSpaceToken :: Token -> Bool
isSpaceToken Space = True
isSpaceToken _ = False

arithm :: Parser [Token]
arithm = many arithmSymbol

arithmSymbol :: Parser Token
arithmSymbol =
  constSymbol
    `or'` leftParen
    `or'` rightParen
    `or'` ops
    `or'` sqrt'
    `or'` vari
    `or'` boolOps
    `or'` space

space :: Parser Token
space = char ' ' `bind` \_ -> result Space

vari :: Parser Token
vari =
  identifier `bind` \i ->
    result (Variable i)

sqrt' :: Parser Token
sqrt' =
  string "SQRT" `bind` \_ ->
    result Sqrt

leftParen :: Parser Token
leftParen =
  char '(' `bind` \_ ->
    result LeftParen

rightParen :: Parser Token
rightParen =
  char ')' `bind` \_ ->
    result RightParen

ops :: Parser Token
ops = plusOp `or'` minusOp `or'` multOp `or'` divOp

plusOp :: Parser Token
plusOp =
  char '+' `bind` \_ ->
    result Plus

minusOp :: Parser Token
minusOp =
  char '-' `bind` \_ ->
    result Minus

multOp :: Parser Token
multOp =
  char '*' `bind` \_ ->
    result Mult

divOp :: Parser Token
divOp =
  char '/' `bind` \_ ->
    result Div

constSymbol :: Parser Token
constSymbol =
  (float' `or'` naturalAsFloat)
    `bind` \v ->
      result (Const v)

eqOp :: Parser Token
eqOp =
  string ".EQ." `bind` \_ ->
    result Equals

neOp :: Parser Token
neOp =
  string ".NE." `bind` \_ ->
    result NotEquals

ltOp :: Parser Token
ltOp =
  string ".LT." `bind` \_ ->
    result Less

leOp :: Parser Token
leOp =
  string ".LE." `bind` \_ ->
    result LessEquals

gtOp :: Parser Token
gtOp =
  string ".GT." `bind` \_ ->
    result Greater

geOp :: Parser Token
geOp =
  string ".GE." `bind` \_ ->
    result GreaterEquals

boolOps :: Parser Token
boolOps =
  eqOp
    `or'` neOp
    `or'` ltOp
    `or'` leOp
    `or'` gtOp
    `or'` geOp
