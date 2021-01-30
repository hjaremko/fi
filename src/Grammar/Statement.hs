-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Grammar.Statement where

import Grammar.Expression

type Variable = String
type Label = Int

data Expr = FloatLiteral Float
    | VarId Variable
    | Arithm [Token]
    deriving Show
        
data Printable = Expr Expr | Str String deriving Show

data Statement = Assignment Variable Expr
    | Print [Printable]
    | Read Variable
    | LabeledStmt Label Statement
    | Loop Statement Expr Expr
    | Continue
    | LabelStmt Label Statement
    | Goto Label
    | If Expr Label Label Label
    deriving Show
