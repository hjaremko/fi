module Grammar where

-- data Indentificator = Ident String 

data Variable = Ident String deriving Show
type Label = Int

data Expr = FloatLiteral Float
    | Add Expr Expr
    deriving Show
        
-- data BooleanExpr = True | False deriving Show

data Statement = Assignment Variable Expr
    | PrintExpr Expr
    | PrintVar Variable
    | Read Variable
    | LabeledStmt Label Statement
    | Loop Statement Expr Expr [Statement]
    | LabelStmt Label Statement
    | Goto Label
    deriving Show