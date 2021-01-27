module Grammar.Grammar where

-- data Indentificator = Ident String 

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
  deriving (Show)

-- data Op
--   = LeftParen Int
--   | Plus Int
priority :: Token -> Int
priority LeftParen = 0
priority Plus = 1
priority Minus = 1
priority RightParen = 1
priority Mult = 2
priority Div = 2

toRpn :: [Token] -> [Token] -> [Token] -> [Token]
toRpn [] out stack = out ++ stack
toRpn (Const v:xs) out stack = toRpn xs (out ++ [Const v]) stack
toRpn (Variable i:xs) out stack = toRpn xs (out ++ [Variable i]) stack
toRpn (LeftParen:xs) out stack =  toRpn xs out (LeftParen:stack)
toRpn (RightParen:xs) out stack =
  let os = popUntilLeftParen out stack
  in uncurry (toRpn xs) os

toRpn (op:ops) out [] = 
    toRpn ops out [op]

toRpn (op:ops) out (s:stack) = 
  if priority op > priority s then
    toRpn ops out (op:s:stack)
  else 
    let os = popHigherOrEqualPriorityOps out (s:stack) op
    in uncurry (toRpn ops) os



popHigherOrEqualPriorityOps :: [Token] -> [Token] -> Token -> ([Token], [Token])
popHigherOrEqualPriorityOps out [] t = (out, [t])
popHigherOrEqualPriorityOps out (op:stack) t =
  if priority op >= priority t then
    popHigherOrEqualPriorityOps (out ++ [op]) stack t
  else (out, t:op:stack)

popUntilLeftParen :: [Token] -> [Token] -> ([Token], [Token])
popUntilLeftParen out (LeftParen:stack) = (out, stack)
popUntilLeftParen out (s:stack) = popUntilLeftParen (out ++ [s]) stack


-- [Const 12.0,Plus,Variable "a",Mult,LeftParen,Variable "b",Mult,Variable "c",Plus,Variable "d",Div,Variable "e",RightParen]

-- [] [Mult, Mult, Mult, Mult, Mult, LeftParen, Mult]
-- [] [Mult, LeftParen, Mult, Plus]

-- [Const 2.0,Const 7.0,Plus,Const 3.0,Div,Const 14.0,Const 3.0,Minus,Const 4.0,Mult,Plus,Const 2.0,Div]

-- data Var = Var String Float deriving Show
-- data State = St [Var] deriving Show

-- evalRpn :: [Token] -> [Float] -> State -> Float
-- evalRpn [] [s] _ = s
-- evalRpn (Const v:ops) stack = evalRpn ops (v:stack)
-- evalRpn (Variable v:ops) stack = evalRpn ops (0:stack)
-- evalRpn (Plus:ops) (a:b:stack) = evalRpn ops ((b + a):stack)
-- evalRpn (Minus:ops) (a:b:stack) = evalRpn ops ((b - a):stack)
-- evalRpn (Mult:ops) (a:b:stack) = evalRpn ops ((b * a):stack)
-- evalRpn (Div:ops) (a:b:stack) = evalRpn ops ((b / a):stack)


type Variable = String
type Label = Int

-- evalExpr :: Expr -> Float
-- evalExpr (FloatLiteral x) = x
-- evalExpr (VarId) = 
-- evalExpr (Add e e') = evalExpr e + evalExpr e'
-- evalExpr (Diff e e') = eval e - eval e'
-- evalExpr (Mult e e') = eval e * eval e'

data Expr = FloatLiteral Float
    | VarId Variable
    | Arithm [Token]
    deriving Show
        
-- data BooleanExpr = True | False deriving Show

data Printable = Expr Expr | PVar Variable | Str String deriving Show

data Statement = Assignment Variable Expr
    | Print [Printable]
    | Read Variable
    | LabeledStmt Label Statement
    | Loop Statement Expr Expr [Statement]
    | LabelStmt Label Statement
    | Goto Label
    | If Expr Label Label Label
    | End
    deriving Show