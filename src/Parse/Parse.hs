-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

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
