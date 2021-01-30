-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Parse.Identifier where

import Parse.Primitive

identificator :: Parser String
identificator =
  letter `bind` \x ->
    many alphanum `bind` \xs ->
      result (x : xs)
