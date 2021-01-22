module Grammar where

-- data Indentificator = Ident String 

data Variable = Ident String deriving Show

data Expr = FloatLiteral Float
    deriving Show
        

data Statement = Assignment Variable Expr
    | PrintExpr Expr
    | PrintVar Variable
    | Read Variable
    deriving Show