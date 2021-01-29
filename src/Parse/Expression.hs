module Parse.Expression where

import Grammar.Grammar
import Parse.Identifier
import Parse.Numbers
import Parse.Primitive

expression :: Parser Expr
expression = arithm `bind` \a -> result (Arithm (removeSpaceToken a))

removeSpaceToken :: [Token] -> [Token]
removeSpaceToken = filter (not . isSpaceToken)

isSpaceToken :: Token -> Bool
isSpaceToken Space = True
isSpaceToken _ = False

arithm :: Parser [Token]
arithm = many arithmSymbol

arithmSymbol :: Parser Token
arithmSymbol =
  constSymbol
    `plus` leftParen
    `plus` rightParen
    `plus` ops
    `plus` sqrt'
    `plus` vari
    `plus` boolOps
    `plus` space

space :: Parser Token
space = char ' ' `bind` \_ -> result Space

-- many (char ' ') `bind` \_ -> zero

-- consumeLeadingSpaces' :: Parser a -> Parser a
-- consumeLeadingSpaces' parser =
--   spaces' `bind` \x ->
--     parser `bind` \xs ->
--       result xs

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
  char '(' `bind` \p ->
    result LeftParen

rightParen :: Parser Token
rightParen =
  char ')' `bind` \p ->
    result RightParen

ops :: Parser Token
ops = plusOp `plus` minusOp `plus` multOp `plus` divOp

plusOp :: Parser Token
plusOp =
  char '+' `bind` \p ->
    result Plus

minusOp :: Parser Token
minusOp =
  char '-' `bind` \p ->
    result Minus

multOp :: Parser Token
multOp =
  char '*' `bind` \p ->
    result Mult

divOp :: Parser Token
divOp =
  char '/' `bind` \p ->
    result Div

constSymbol :: Parser Token
constSymbol =
  (float' `plus` naturalF)
    `bind` \v ->
      result (Const v)

eqOp :: Parser Token
eqOp =
  string ".EQ." `bind` \p ->
    result Equals

neOp :: Parser Token
neOp =
  string ".NE." `bind` \p ->
    result NotEquals

ltOp :: Parser Token
ltOp =
  string ".LT." `bind` \p ->
    result Less

leOp :: Parser Token
leOp =
  string ".LE." `bind` \p ->
    result LessEquals

gtOp :: Parser Token
gtOp =
  string ".GT." `bind` \p ->
    result Greater

geOp :: Parser Token
geOp =
  string ".GE." `bind` \p ->
    result GreaterEquals

boolOps :: Parser Token
boolOps =
  eqOp
    `plus` neOp
    `plus` ltOp
    `plus` leOp
    `plus` gtOp
    `plus` geOp
