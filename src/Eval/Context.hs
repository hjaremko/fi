-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Eval.Context where

import Grammar.Statement

data Var = Var String Float deriving (Show)

type State = [Var]

type JumpData = [(Label, Int)]

type Context = (State, JumpData, [Statement])

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

varValue :: String -> State -> Float
varValue name vars = findVarValue vars name

getIdx :: Label -> JumpData -> [Statement] -> Int
getIdx label jd all = head $ map snd (filter (\x -> label == fst x) jd)
