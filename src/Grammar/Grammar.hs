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
  | Equals
  | NotEquals
  | Less
  | LessEquals
  | Greater
  | GreaterEquals
  | Space
  deriving (Show)

priority :: Token -> Int
priority LeftParen = 0
priority Equals = 8
priority NotEquals = 8
priority Less = 8
priority LessEquals = 8
priority Greater = 8
priority GreaterEquals = 8
priority Plus = 12
priority Minus = 12
priority RightParen = 12
priority Mult = 13
priority Div = 13
priority Sqrt = 14
priority UnaryMinus = 15

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
    | Loop Statement Expr Expr
    | Continue
    | LabelStmt Label Statement
    | Goto Label
    | If Expr Label Label Label
    deriving Show
