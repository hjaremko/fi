-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu
module Parse.Parse where

import Grammar.Statement
import Parse.Primitive
import Parse.Statement

parseHelp :: String -> [(Statement, String)] -> [(Statement, String)]
parseHelp "" results = results
parseHelp code results = results ++ parseOne code ++ parseHelp (leftover $ parseOne code) results
  where
    parseOne = first statement
    leftover [] = []
    leftover (x : xs) = snd x

parse :: String -> [Statement]
parse s = map fst (parseHelp s [])
