module Parse.If where

import Grammar.Grammar
import Parse.Expression
import Parse.Numbers
import Parse.Primitive

if' :: Parser Statement
if' =
  string "IF" `bind` \x ->
    spaces `bind` \y ->
      char '(' `bind` \y ->
        spaces `bind` \y ->
          expression `bind` \expr ->
            spaces `bind` \y ->
              char ')' `bind` \y ->
                spaces `bind` \y ->
                  many digit `bind` \neg ->
                    spaces `bind` \y ->
                      char ',' `bind` \y ->
                        spaces `bind` \y ->
                          many digit `bind` \zero ->
                            spaces `bind` \y ->
                              char ',' `bind` \y ->
                                spaces `bind` \y ->
                                  many digit `bind` \pos ->
                                    spaces `bind` \y ->
                                      result (If expr (read neg) (read zero) (read pos))

iff :: Parser Statement
iff = first $ consumeLeadingSpaces if'
