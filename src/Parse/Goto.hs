-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Parse.Goto where

import Grammar.Statement
import Parse.Numbers
import Parse.Primitive

goto' :: Parser Statement
goto' =
  string "GOTO" `bind` \_ ->
    whitespaces `bind` \_ ->
      manyNotEmpty digit `bind` \label ->
        result (Goto (read label))

goto :: Parser Statement
goto = first $ consumeLeadingSpaces goto'
