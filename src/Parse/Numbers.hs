module Parse.Numbers where

import Grammar.Grammar (Expr (FloatLiteral))
import Parse.Primitive

float' :: Parser Float
float' =
  manyNotEmpty digit `bind` \x ->
    char '.' `bind` \y ->
      manyNotEmpty digit `bind` \xs ->
        result (read (x ++ [y] ++ xs))

float :: Parser Expr
float =
  ( float' `bind` \n ->
      result (FloatLiteral n)
  )
    `plus` ( char '-' `bind` \m ->
               float' `bind` \num ->
                 result (FloatLiteral (- num))
           )

natural :: Parser Int
natural =
  manyNotEmpty digit `bind` \x ->
    result (read x)

    
naturalF :: Parser Float
naturalF =
  manyNotEmpty digit `bind` \x ->
    result (read x)

int :: Parser Expr
int =
  ( natural `bind` \n ->
      result (FloatLiteral (fromIntegral n))
  )
    `plus` ( char '-' `bind` \m ->
               natural `bind` \num ->
                 result (FloatLiteral (fromIntegral (- num)))
           )
