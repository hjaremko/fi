-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Eval.Eval where

import Eval.Context
import Eval.Rpn
import Grammar.Statement

isInitialized :: [Var] -> String -> Bool
isInitialized [] _ = False
isInitialized (Var id val : vs) name = (id == name) || isInitialized vs name

evalAssignment :: State -> String -> Float -> State
evalAssignment vars name val =
  if isInitialized vars name
    then changeValue
    else addNewVariable
  where
    changeValue = map (\(Var id v) -> if id == name then Var id val else Var id v) vars
    addNewVariable = Var name val : vars

evalExpr :: Expr -> State -> Float
evalExpr (FloatLiteral x) _ = x
evalExpr (VarId i) state = varValue i state
evalExpr (Arithm tokens) state = evalRpn (toRpn tokens [] []) [] state

evalOne :: Statement -> Context -> IO State
evalOne (Assignment varName expr) (state, jd, all) = return (evalAssignment state varName (evalExpr expr state))
evalOne (Print []) (state, _, _) = do
  putStr "\n"
  return state
evalOne (Print (Expr e : xs)) (state, jd, all) = do
  putStr $ show $ evalExpr e state
  evalOne (Print xs) (state, jd, all)
evalOne (Print (Str s : xs)) ctx = do
  putStr s
  evalOne (Print xs) ctx
evalOne (Read varName) (state, jd, all) = evalAssignment state varName . read <$> getLine
evalOne (Loop init _ _) ctx = evalOne init ctx
evalOne (LabelStmt _ stmt) ctx = evalOne stmt ctx

evalGoto :: Label -> JumpData -> [Statement] -> [Statement]
evalGoto label jd all = drop (getIdx label jd all -1) all

eval :: [Statement] -> Context -> IO State
eval [] (s, _, _) = return s
eval (Goto label : _) (state, jd, all) =
  eval (evalGoto label jd all) (state, jd, all)
eval (LabelStmt label Continue : rest) (state, jd, all) = do
  let (LabelStmt _ (Loop (Assignment itName assignExpr) stopE stepE)) = all !! (getIdx label jd all - 1)
  let val = varValue itName state
  let stop = evalExpr stopE state
  let step = evalExpr stepE state
  s' <- evalOne (Assignment itName (FloatLiteral (val + step))) (state, jd, all)

  let val' = varValue itName s'

  if val' < stop
    then eval (drop (getIdx label jd all) all) (s', jd, all)
    else eval rest (s', jd, all)

eval (If expr neg zero pos : _) (state, jd, all)
  | evalExpr expr state < 0 = eval (evalGoto neg jd all) (state, jd, all)
  | evalExpr expr state == 0 = eval (evalGoto zero jd all) (state, jd, all)
  | evalExpr expr state > 0 = eval (evalGoto pos jd all) (state, jd, all)

eval (statement : rest) (state, jd, all) = do
  r <- evalOne statement (state, jd, all)
  eval rest (r, jd, all)
