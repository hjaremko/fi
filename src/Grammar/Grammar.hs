module Grammar.Grammar where

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
  deriving (Show)

priority :: Token -> Int
priority LeftParen = 0
priority Plus = 1
priority Minus = 1
priority RightParen = 1
priority Mult = 2
priority Div = 2
priority Sqrt = 3
priority UnaryMinus = 4

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
    | Loop Statement Expr Expr [Statement]
    | LabelStmt Label Statement
    | Goto Label
    | If Expr Label Label Label
    | End
    deriving Show
