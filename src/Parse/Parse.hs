-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu
module Parse.Parse where

import Grammar.Statement
import Parse.Primitive
import Parse.Statement

parse :: String -> [Statement]
-- parse code = fst (head (many statement code))
-- parse code = fst (head ((many statement) code))
-- parse code = fst ((head .(many statement)) code)
-- parse code = (fst . head . (many statement)) code
parse = fst . head . many statement
