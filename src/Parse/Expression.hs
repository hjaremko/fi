module Parse.Expression where

import Grammar.Grammar
import Parse.Identifier
import Parse.Numbers
import Parse.Primitive

expression :: Parser Expr
expression = arithm `bind` \a -> result (Arithm a)

arithm :: Parser [Token]
arithm =
  many
    ( constSymbol
        `plus` leftParen
        `plus` rightParen
        `plus` ops
        `plus` sqrt'
        `plus` vari
        `plus` boolOps
    )

vari :: Parser Token
vari =
  identificator `bind` \i ->
    result (Variable i)

sqrt' :: Parser Token
sqrt' =
  string "SQRT" `bind` \i ->
    result Sqrt

leftParen :: Parser Token
leftParen =
  consumeIf (== '(') `bind` \p ->
    result LeftParen

rightParen :: Parser Token
rightParen =
  consumeIf (== ')') `bind` \p ->
    result RightParen

ops :: Parser Token
ops = plusOp `plus` minusOp `plus` multOp `plus` divOp

plusOp :: Parser Token
plusOp =
  consumeIf (== '+') `bind` \p ->
    result Plus

minusOp :: Parser Token
minusOp =
  consumeIf (== '-') `bind` \p ->
    result Minus

multOp :: Parser Token
multOp =
  consumeIf (== '*') `bind` \p ->
    result Mult

divOp :: Parser Token
divOp =
  consumeIf (== '/') `bind` \p ->
    result Div

constSymbol :: Parser Token
constSymbol =
  first (float' `plus` naturalF)
    `bind` \v ->
      result (Const v)

eqOp :: Parser Token
eqOp =
  string ".EQ." `bind` \p ->
    result Equals

neOp :: Parser Token
neOp =
  string ".NE." `bind` \p ->
    result Equals

ltOp :: Parser Token
ltOp =
  string ".LT." `bind` \p ->
    result Equals

leOp :: Parser Token
leOp =
  string ".LE." `bind` \p ->
    result Equals

gtOp :: Parser Token
gtOp =
  string ".GT." `bind` \p ->
    result Equals

geOp :: Parser Token
geOp =
  string ".GE." `bind` \p ->
    result Equals

boolOps :: Parser Token
boolOps =
  eqOp
    `plus` neOp
    `plus` ltOp
    `plus` leOp
    `plus` gtOp
    `plus` geOp
