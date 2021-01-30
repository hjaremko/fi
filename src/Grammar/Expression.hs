-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Grammar.Expression where

data Token
  = Const Float
  | Plus
  | Minus
  | Mult
  | Div
  | Sqrt
  | Variable String
  | LeftParen
  | RightParen
  | UnaryMinus
  | Equals
  | NotEquals
  | Less
  | LessEquals
  | Greater
  | GreaterEquals
  | Space
  deriving (Show)
