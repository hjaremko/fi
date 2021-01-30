-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Main where

import Parse.Parse
import Eval.Context
import Eval.Eval
import System.Environment
import System.IO
import Prelude

main :: IO ()
main = do
  (filename : _) <- getArgs
  fileContent <- readFile filename

  let allStatements = parse fileContent
  let jumpData = readJumpData allStatements
  let context = ([], jumpData, allStatements)

  -- print allStatements
  -- print jumpData
  eval allStatements context
  return ()
