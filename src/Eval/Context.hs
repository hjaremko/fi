-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Eval.Context where

import Grammar.Statement

data Var = Var String Float deriving (Show)

type State = [Var]

type JumpData = [(Label, Int)]

type Context = (State, JumpData, [Statement])

readJumpData :: [Statement] -> JumpData
-- readJumpData statements = filter hasLabel (concatMap toJumpData (addIndex statements))
-- readJumpData statements = filter hasLabel (((concatMap toJumpData) .addIndex) statements)
-- readJumpData  = ((filter hasLabel) .((concatMap toJumpData) .addIndex))
readJumpData = filter hasLabel . (concatMap toJumpData . addIndex)
  where
    hasLabel (_, i) = i /= -1
    toJumpData (i, LabelStmt label _) = [(label, i)]
    toJumpData _ = [(0, -1)]
    addIndex = zip [1 ..]

varValue :: String -> State -> Float
varValue _ [] = 0
varValue name (Var id val : vars) =
  if id == name
    then val
    else varValue name vars

getIdx :: Label -> JumpData -> [Statement] -> Int
getIdx label jd all = head $ map snd (filter (\x -> label == fst x) jd)
