-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Parse.Assignment where

import Grammar.Statement
import Parse.Expression
import Parse.Identifier
import Parse.Primitive

assignment' :: Parser Statement
assignment' =
  identifier `bind` \id ->
    whitespaces `bind` \_ ->
      char '=' `bind` \_ ->
        whitespaces `bind` \_ ->
          expression `bind` \expr ->
            result (Assignment id expr)

assignment :: Parser Statement
assignment = first $ consumeLeadingSpaces assignment'
