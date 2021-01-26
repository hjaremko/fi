module Grammar.Grammar where

-- data Indentificator = Ident String 

type Variable = String
type Label = Int

evalExpr :: Expr -> Float
evalExpr (FloatLiteral x) = x
evalExpr (Add e e') = evalExpr e + evalExpr e'
-- evalExpr (Diff e e') = eval e - eval e'
-- evalExpr (Mult e e') = eval e * eval e'

data Expr = FloatLiteral Float
    | Add Expr Expr
    deriving Show
        
-- data BooleanExpr = True | False deriving Show

data Printable = Expr Expr | PVar Variable | Str String deriving Show

data Statement = Assignment Variable Expr
    -- | PrintExpr Expr
    -- | PrintVar Variable
    | Print [Printable]
    | Read Variable
    | LabeledStmt Label Statement
    | Loop Statement Expr Expr [Statement]
    | LabelStmt Label Statement
    | Goto Label
    | If Expr Label Label Label
    deriving Show