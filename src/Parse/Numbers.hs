module Parse.Numbers where

import Grammar.Grammar (Expr (FloatLiteral))
import Parse.Primitive

-- todo: ujemne
float :: Parser Expr
float =
  manyNotEmpty digit `bind` \x ->
    char '.' `bind` \y ->
      manyNotEmpty digit `bind` \xs ->
        result (FloatLiteral (read (x ++ [y] ++ xs)))

int :: Parser Expr
int =
  manyNotEmpty digit `bind` \x ->
    result (FloatLiteral (read x))
