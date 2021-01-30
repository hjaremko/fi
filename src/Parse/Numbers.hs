-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu
module Parse.Numbers where

import Grammar.Statement
import Parse.Primitive

float' :: Parser Float
float' =
  manyNotEmpty digit `bind` \beforeDot ->
    char '.' `bind` \dot ->
      manyNotEmpty digit `bind` \afterDot ->
        result (read (beforeDot ++ [dot] ++ afterDot))

float :: Parser Expr
float =
  ( float' `bind` \n ->
      result (FloatLiteral n)
  )
    `or'` ( char '-' `bind` \_ ->
              float' `bind` \num ->
                result (FloatLiteral (- num))
          )

natural :: Parser Int
natural =
  manyNotEmpty digit `bind` \x ->
    result (read x)

naturalAsFloat :: Parser Float
naturalAsFloat =
  manyNotEmpty digit `bind` \x ->
    result (read x)

int :: Parser Expr
int =
  ( natural `bind` \n ->
      result (FloatLiteral (fromIntegral n))
  )
    `or'` ( char '-' `bind` \m ->
              natural `bind` \num ->
                result (FloatLiteral (fromIntegral (- num)))
          )
