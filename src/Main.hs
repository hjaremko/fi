-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

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
