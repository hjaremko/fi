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
    -- (evalAssignment state id (read n), return())
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

-- evalOne :: Statement -> State -> (State, IO())
-- evalOne (Assignment (Ident id) (FloatLiteral val)) state = (evalAssignment state id val, return ())
-- evalOne (PrintExpr expr) state = (state, print $ evalExpr expr)
-- evalOne (PrintVar (Ident id)) state = (state, print $ varValue id state)
-- evalOne (Read (Ident id)) state = 
--     (evalAssignment state id (read (n)), do n <- getLine; return ())
        -- (evalAssignment state id (read (n)), return ())

eval :: [Statement] -> State -> IO State
eval [] s = return s
eval (parseResult:rest) state = do
    r <- evalOne parseResult state
    -- snd r
    eval rest r
    -- eval rest $ fst r

main :: IO ()
main = do
  (filename : _) <- getArgs
  fileContent <- readFile filename
  let parsed = parse fileContent
  let state = St []
  eval parsed state
  print parsed
