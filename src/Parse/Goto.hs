module Parse.Goto where

import Grammar.Grammar
import Parse.Numbers
import Parse.Primitive

goto' :: Parser Statement
goto' =
  string "GOTO" `bind` \x ->
    spaces `bind` \y ->
      many digit `bind` \label ->
        result (Goto $ read label)

goto :: Parser Statement
goto = first $ consumeLeadingSpaces goto'
