module Parse.Expression where

import Grammar.Grammar (Expr)
import Parse.Primitive
import Parse.Numbers

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

-- `plus` result (FloatLiteral 0)
