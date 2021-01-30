-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Parse.Identifier where

import Parse.Primitive

-- Wszystkie alfanumeryczne, ale zaczynajace sie od litery
identifier :: Parser String
identifier =
  letter `bind` \x ->
    many alphanum `bind` \xs ->
      result (x : xs)
