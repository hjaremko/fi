module Grammar where

-- data Indentificator = Ident String 

data Variable = Ident String deriving Show
data Label = Label Int deriving Show

data Expr = FloatLiteral Float
    deriving Show
        
-- data BooleanExpr = True | False deriving Show

data Statement = Assignment Variable Expr
    | PrintExpr Expr
    | PrintVar Variable
    | Read Variable
    | LabeledStmt Label Statement
    | Loop Statement Expr Expr [Statement]
    | LabelStmt Label Statement
    deriving Show