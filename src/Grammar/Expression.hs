-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

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
