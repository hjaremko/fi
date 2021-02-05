-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Parse.Identifier where

import Parse.Primitive

-- Wszystkie alfanumeryczne, ale zaczynajace sie od litery
identifier :: Parser String
identifier =
  letter `bind` \x ->
    many alphanum `bind` \xs ->
      result (x : xs)
