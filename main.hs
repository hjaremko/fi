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
type JumpData =  [(Label, Int)]-- deriving Show

readJumpData ::[Statement] -> JumpData
readJumpData sts = filter (\(_,i) -> i /= -1) (map toJumpData (addIndex sts))
    where toJumpData (i, LabelStmt label _) = (label, i)
          toJumpData _ = (0, -1)
          addIndex = zip [1..] 

findVarValue :: [Var] -> String -> Float
findVarValue [] _ = 0;
findVarValue ((Var id val):vs) name = if id == name then val
                                      else findVarValue vs name

isInitialized :: [Var] -> String -> Bool
isInitialized [] _ = False;
isInitialized ((Var id val):vs) name = if id == name then True
                                      else isInitialized vs name

-- evalExpr :: Expr -> Float
-- evalExpr (FloatLiteral val) = val

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
evalOne (Assignment (Ident id) (FloatLiteral val)) state jd all
    = return (evalAssignment state id val)
    
evalOne (PrintExpr expr) state jd all = do
    print $ evalExpr expr
    return state
    
evalOne (PrintVar (Ident id)) state jd all = do
    print $ varValue id state
    return state
    
evalOne (Read (Ident id)) state jd all = do
    n <- getLine
    return (evalAssignment state id (read n))
    
evalOne (Loop (Assignment id (FloatLiteral val)) (FloatLiteral stop) (FloatLiteral step) stmts) state jd all = do
    s <- evalOne (Assignment id (FloatLiteral val)) state jd all
    if (val < stop) then do
        s' <- eval stmts s jd all
        let nextLoop = Loop (Assignment id (FloatLiteral (val + step))) (FloatLiteral stop) (FloatLiteral step) stmts
        evalOne nextLoop s' jd all
    else do 
        return s
        
evalOne (LabelStmt _ stmt) state jd all = evalOne stmt state jd all

evalGoto :: Label -> JumpData -> [Statement] -> [Statement]
evalGoto label jd all = drop (getIdx -1) all
    where getIdx = head $ map snd $ filter (\(l,i)-> l == label) jd
    
eval :: [Statement] -> State -> JumpData -> [Statement] -> IO State
eval [] s _ _ = return s

eval (Goto label:_) state jd all = do
    s <- eval (evalGoto label jd all)  state jd all
    return s

eval (If expr neg zero pos:_) state jd all 
 | evalExpr expr < 0 = eval (evalGoto neg jd all) state jd all
 | evalExpr expr == 0 = eval (evalGoto zero jd all)  state jd all
 | evalExpr expr > 0 =    eval (evalGoto pos jd all) state jd all

eval (statement:rest) state jd all = do
    r <- evalOne statement state jd all
    eval rest r jd all

main :: IO ()
main = do
  (filename : _) <- getArgs
  fileContent <- readFile filename

  let parsed = parse fileContent
  print parsed

  let jd = readJumpData parsed
  print jd
  
  let state = St []
  eval parsed state jd parsed

  return ()