-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Prosty interpreter Fortranu

module Main where

import System.Environment -- importujemy ten modul po to, aby skorzystac z funkcji getArgs
import System.IO
import Prelude
import Grammar.Grammar
import Parse.Primitive
import Parse.Statement

parseHelp :: String -> [(Statement, String)] -> [(Statement, String)]
parseHelp "" results = results
parseHelp code results = results ++ parseOne code ++ parseHelp (leftover $ parseOne code) results
    where parseOne = first statement
          leftover [] = []
          leftover (x:xs) = snd x

parse :: String -> [Statement]
parse s = map fst (parseHelp s [])

data Var = Var String Float deriving Show
type State = [Var]
-- data State = St [Var] [LabelStmt] deriving Show
type JumpData =  [(Label, Int)]-- deriving Show
data Context = Ctx State JumpData [Statement]

-- toJumpData (i, Loop _ _ _ stmts) = readJumpData stmts

concatInnerStatements :: [Statement] -> [Statement]
concatInnerStatements [] = []
concatInnerStatements (Loop s _ _ ss:xs) = s:concatInnerStatements ss ++ concatInnerStatements xs
concatInnerStatements (x:xs) = x:concatInnerStatements xs
-- concatInnerStatements (x:xs) 


readJumpData ::[Statement] -> JumpData
-- readJumpData (Loop _ _ _ ss:xs) = readJumpData ss ++ readJumpData xs
readJumpData sts = filter (\(_,i) -> i /= -1) (concatMap toJumpData (addIndex sts))
    where 
        --   toJumpData (i, LabelStmt label (Loop _ _ _ ss)) = (label, i):(readJumpData ss)
          toJumpData (i, LabelStmt label _) = [(label, i)]
          toJumpData _ = [(0, -1)]
          addIndex = zip [1..] 

findVarValue :: [Var] -> String -> Float
findVarValue [] _ = 0;
findVarValue (Var id val:vs) name = if id == name then val
                                      else findVarValue vs name

isInitialized :: [Var] -> String -> Bool
isInitialized [] _ = False;
isInitialized (Var id val:vs) name = (id == name) || isInitialized vs name

-- evalExpr :: Expr -> Float
-- evalExpr (FloatLiteral val) = val

evalExpr :: Expr -> State -> Float
evalExpr (FloatLiteral x) _ = x
evalExpr (VarId i) state = varValue i state
evalExpr (Arithm tokens) state = 0

-- evalArithmetic :: [Token] -> State -> Float
-- evalArithmetic tokens state = evalRpn (toRpn tokens [] []) [] state


evalRpn :: [Token] -> [Float] -> State -> Float
evalRpn [] [s] _ = s
evalRpn (Const v:ops) stack state = evalRpn ops (v:stack) state
evalRpn (Variable v:ops) stack state = evalRpn ops (varValue v state:stack) state
evalRpn (Plus:ops) (a:b:stack) state = evalRpn ops ((b + a):stack) state
evalRpn (Minus:ops) (a:b:stack) state = evalRpn ops ((b - a):stack) state
evalRpn (Mult:ops) (a:b:stack) state = evalRpn ops ((b * a):stack) state
evalRpn (Div:ops) (a:b:stack) state = evalRpn ops ((b / a):stack) state


evalAssignment :: State -> String -> Float -> State
evalAssignment (St vars) name val
    = if isInitialized vars name then
            St (map (\(Var id v) -> if id == name then (Var id val) else (Var id v)) vars)
        else
            St((Var name val):vars) 


varValue :: String -> State -> Float
varValue name (St vars) = findVarValue vars name

readSt :: State -> String -> IO State
readSt state id = do 
    n <- getLine
    return (evalAssignment state id (read n))

evalOne :: Statement -> State -> JumpData -> [Statement] -> IO State
evalOne (Assignment id expr) state jd all
    = return (evalAssignment state id (evalExpr expr state))
    
evalOne (Print []) state jd all = do
    putStr "\n"
    return state

evalOne (Print (Expr e:xs)) state jd all = do
    putStr $ show $ evalExpr e state
    evalOne (Print xs) state jd all
    
evalOne (Print (PVar id:xs)) state jd all = do
    putStr $ show $ varValue id state
    evalOne (Print xs) state jd all

evalOne (Print (Str s:xs)) state jd all = do
    putStr s
    evalOne (Print xs) state jd all

evalOne (Read id) state jd all = do
    n <- getLine
    return (evalAssignment state id (read n))
    
evalOne (Loop (LabelStmt _ (Assignment id (FloatLiteral val))) (FloatLiteral stop) (FloatLiteral step) stmts) state jd all =
    evalOne (Loop (Assignment id (FloatLiteral val)) (FloatLiteral stop) (FloatLiteral step) stmts) state jd all

evalOne (Loop (Assignment id (FloatLiteral val)) (FloatLiteral stop) (FloatLiteral step) stmts) state jd all = do
    s <- evalOne (Assignment id (FloatLiteral val)) state jd all
    if (val < stop) then do
        -- print stmts
        s' <- eval stmts s jd all
        let nextLoop = Loop (Assignment id (FloatLiteral (val + step))) (FloatLiteral stop) (FloatLiteral step) stmts
        evalOne nextLoop s' jd all
    else do 
        return s
        
evalOne (LabelStmt _ stmt) state jd all = evalOne stmt state jd all

-- evalOne (LabelStmt _ stmt) state jd all = evalOne stmt state jd all

evalGoto :: Label -> JumpData -> [Statement] -> [Statement]
evalGoto label jd all = drop (getIdx -1) all
    where getIdx = head $ map snd $ filter (\(l,i)-> l == label) jd
    
eval :: [Statement] -> State -> JumpData -> [Statement] -> IO State
eval [] s _ _ = return s

eval (End: _) s _ _ = return s

eval (Goto label:_) state jd all = do
    s <- eval (evalGoto label jd all)  state jd all
    return s

eval (If expr neg zero pos:_) state jd all 
 | evalExpr expr state < 0 = eval (evalGoto neg jd all) state jd all
 | evalExpr expr state == 0 = eval (evalGoto zero jd all)  state jd all
 | evalExpr expr state > 0 =    eval (evalGoto pos jd all) state jd all

eval (statement:rest) state jd all = do
    -- print rest
    r <- evalOne statement state jd all
    eval rest r jd all

main :: IO ()
main = do
  (filename : _) <- getArgs
  fileContent <- readFile filename

  let parsed = parse fileContent
  print parsed

  let allStatements = concatInnerStatements parsed
  let jd = readJumpData allStatements
  print jd
--   print allStatements
  
  let state = St []
  eval parsed state jd allStatements
--   print parsed
  return ()
