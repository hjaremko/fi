-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Prosty interpreter Fortranu

import System.Environment -- importujemy ten modul po to, aby skorzystac z funkcji getArgs
import System.IO
import Prelude
import Grammar
import Parsers

parseHelp :: String -> [(Statement, String)] -> [(Statement, String)]
parseHelp "" results = results
parseHelp code results = results ++ parseOne code ++ parseHelp (leftover $ parseOne code) results
    where parseOne str = first statement str
          leftover [] = []
          leftover (x:xs) = snd x

parse :: String -> [Statement]
parse s = map fst (parseHelp s [])

data Var = Var String Float deriving Show
data State = St [Var] deriving Show
-- data State = St [Var] [LabelStmt] deriving Show

findVarValue :: [Var] -> String -> Float
findVarValue [] _ = 0;
findVarValue ((Var id val):vs) name = if id == name then val
                                      else findVarValue vs name

isInitialized :: [Var] -> String -> Bool
isInitialized [] _ = False;
isInitialized ((Var id val):vs) name = if id == name then True
                                      else isInitialized vs name

evalExpr :: Expr -> Float
evalExpr (FloatLiteral val) = val

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

evalOne :: Statement -> State -> IO State
evalOne (Assignment (Ident id) (FloatLiteral val)) state
    = return (evalAssignment state id val)
evalOne (PrintExpr expr) state = do
    print $ evalExpr expr
    return (state)
evalOne (PrintVar (Ident id)) state = do
    print $ varValue id state
    return (state)
evalOne (Read (Ident id)) state = do
    n <- getLine
    return (evalAssignment state id (read n))
evalOne (Loop (Assignment id (FloatLiteral val)) (FloatLiteral stop) (FloatLiteral step) stmts) state = do
    s <- evalOne (Assignment id (FloatLiteral val)) state
    if (val < stop) then do
        s' <- eval stmts s
        let nextLoop = Loop (Assignment id (FloatLiteral (val + step))) (FloatLiteral stop) (FloatLiteral step) stmts
        evalOne nextLoop s'
    else do 
        return s

-- loop :: 
-- loop :: [Statement] -> State -> IO State
-- loop stmst state = do
--     s <- evalOne start state
--     s' <- eval stmts s
--     return s'
    
eval :: [Statement] -> State -> IO State
eval [] s = return s
eval (parseResult:rest) state = do
    r <- evalOne parseResult state
    eval rest r

main :: IO ()
main = do
  (filename : _) <- getArgs
  fileContent <- readFile filename
  let parsed = parse fileContent
  print parsed
  let state = St []
  eval parsed state
  return ()