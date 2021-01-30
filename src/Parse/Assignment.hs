-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Parse.Assignment where

import Grammar.Statement
import Parse.Expression
import Parse.Identifier
import Parse.Primitive

assignment' :: Parser Statement
assignment' =
  identificator `bind` \x ->
    spaces `bind` \z ->
      char '=' `bind` \y ->
        spaces `bind` \z ->
          expression `bind` \xs ->
            result (Assignment x xs)

assignment :: Parser Statement
assignment = first $ consumeLeadingSpaces assignment'
