-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Main where

import Grammar.Grammar
import Parse.Primitive
import Parse.Statement
import System.Environment
import System.IO
import Prelude

parseHelp :: String -> [(Statement, String)] -> [(Statement, String)]
parseHelp "" results = results
parseHelp code results = results ++ parseOne code ++ parseHelp (leftover $ parseOne code) results
  where
    parseOne = first statement
    leftover [] = []
    leftover (x : xs) = snd x

parse :: String -> [Statement]
parse s = map fst (parseHelp s [])

data Var = Var String Float deriving (Show)

type State = [Var]

type JumpData = [(Label, Int)]

type Context = (State, JumpData, [Statement])

concatInnerStatements :: [Statement] -> [Statement]
concatInnerStatements [] = []
concatInnerStatements (Loop s _ _ ss : xs) = s : concatInnerStatements ss ++ concatInnerStatements xs
concatInnerStatements (x : xs) = x : concatInnerStatements xs

readJumpData :: [Statement] -> JumpData
readJumpData sts = filter (\(_, i) -> i /= -1) (concatMap toJumpData (addIndex sts))
  where
    toJumpData (i, LabelStmt label _) = [(label, i)]
    toJumpData _ = [(0, -1)]
    addIndex = zip [1 ..]

findVarValue :: [Var] -> String -> Float
findVarValue [] _ = 0
findVarValue (Var id val : vs) name =
  if id == name
    then val
    else findVarValue vs name

isInitialized :: [Var] -> String -> Bool
isInitialized [] _ = False
isInitialized (Var id val : vs) name = (id == name) || isInitialized vs name

evalExpr :: Expr -> State -> Float
evalExpr (FloatLiteral x) _ = x
evalExpr (VarId i) state = varValue i state
evalExpr (Arithm tokens) state = evalRpn (toRpn tokens [] []) [] state

evalRpn :: [Token] -> [Float] -> State -> Float
evalRpn [] [s] _ = s
evalRpn (Const v : ops) stack state = evalRpn ops (v : stack) state
evalRpn (Variable v : ops) stack state = evalRpn ops (varValue v state : stack) state
evalRpn (Plus : ops) (a : b : stack) state = evalRpn ops ((b + a) : stack) state
evalRpn (Minus : ops) (a : b : stack) state = evalRpn ops ((b - a) : stack) state
evalRpn (Mult : ops) (a : b : stack) state = evalRpn ops ((b * a) : stack) state
evalRpn (Div : ops) (a : b : stack) state = evalRpn ops ((b / a) : stack) state
evalRpn (Sqrt : ops) (a : stack) state = evalRpn ops (sqrt a : stack) state
evalRpn (Equals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b == a) : stack) state
evalRpn (NotEquals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b /= a) : stack) state
evalRpn (Less : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b < a) : stack) state
evalRpn (LessEquals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b <= a) : stack) state
evalRpn (Greater : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b > a) : stack) state
evalRpn (GreaterEquals : ops) (a : b : stack) state = evalRpn ops (boolToFloat (b >= a) : stack) state

boolToFloat :: Bool -> Float
boolToFloat op = if op then 1.0 else 0.0

evalAssignment :: State -> String -> Float -> State
evalAssignment vars name val =
  if isInitialized vars name
    then map (\(Var id v) -> if id == name then Var id val else Var id v) vars
    else Var name val : vars

varValue :: String -> State -> Float
varValue name vars = findVarValue vars name

readSt :: State -> String -> IO State
readSt state id = evalAssignment state id . read <$> getLine

evalOne :: Statement -> Context -> IO State
evalOne (Assignment id expr) (state, jd, all) =
  return (evalAssignment state id (evalExpr expr state))
evalOne (Print []) (state, _, _) = do
  putStr "\n"
  return state
evalOne (Print (Expr e : xs)) (state, jd, all) = do
  putStr $ show $ evalExpr e state
  evalOne (Print xs) (state, jd, all)
evalOne (Print (Str s : xs)) ctx = do
  putStr s
  evalOne (Print xs) ctx
evalOne (Read id) (state, jd, all) =
  evalAssignment state id . read <$> getLine
evalOne (Loop (LabelStmt _ assign) stop step stmts) ctx =
  evalOne (Loop assign stop step stmts) ctx
evalOne (Loop (Assignment id assignExpr) stopExpr stepExpr stmts) (state, jd, all) = do
  let val = evalExpr assignExpr state
  let stop = evalExpr stopExpr state
  let step = evalExpr stepExpr state
  s <- evalOne (Assignment id (FloatLiteral val)) (state, jd, all)
  if val < stop
    then do
      s' <- eval stmts (s, jd, all)
      let nextLoop = Loop (Assignment id (FloatLiteral (val + step))) (FloatLiteral stop) (FloatLiteral step) stmts
      evalOne nextLoop (s', jd, all)
    else return s
evalOne (LabelStmt _ stmt) ctx = evalOne stmt ctx

evalGoto :: Label -> JumpData -> [Statement] -> [Statement]
evalGoto label jd = drop (getIdx -1)
  where
    getIdx = head . map snd $ filter (\(l, i) -> l == label) jd

eval :: [Statement] -> Context -> IO State
eval [] (s, _, _) = return s
eval (End : _) (s, _, _) = return s
eval (Goto label : _) (state, jd, all) =
  eval (evalGoto label jd all) (state, jd, all)
eval (If expr neg zero pos : _) (state, jd, all)
  | evalExpr expr state < 0 = eval (evalGoto neg jd all) (state, jd, all)
  | evalExpr expr state == 0 = eval (evalGoto zero jd all) (state, jd, all)
  | evalExpr expr state > 0 = eval (evalGoto pos jd all) (state, jd, all)
eval (statement : rest) (state, jd, all) = do
  r <- evalOne statement (state, jd, all)
  eval rest (r, jd, all)

main :: IO ()
main = do
  (filename : _) <- getArgs
  fileContent <- readFile filename

  let parsed = parse fileContent
  let allStatements = concatInnerStatements parsed
  let jd = readJumpData allStatements
  let ctx = ([], jd, allStatements)

  print allStatements
  print jd
  eval parsed ctx
  return ()
