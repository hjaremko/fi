-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Parse.Goto where

import Grammar.Statement
import Parse.Numbers
import Parse.Primitive

goto' :: Parser Statement
goto' =
  string "GOTO" `bind` \x ->
    spaces `bind` \y ->
      manyNotEmpty digit `bind` \label ->
        result (Goto $ read label)

goto :: Parser Statement
goto = first $ consumeLeadingSpaces goto'
