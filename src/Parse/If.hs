-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Parse.If where

import Grammar.Statement
import Parse.Expression
import Parse.Identifier
import Parse.Numbers
import Parse.Primitive

if' :: Parser Statement
if' =
  string "IF" `bind` \_ ->
    whitespaces `bind` \_ ->
      char '(' `bind` \_ ->
        whitespaces `bind` \_ ->
          expression `bind` \expr ->
            whitespaces `bind` \_ ->
              char ')' `bind` \_ ->
                whitespaces `bind` \_ ->
                  many digit `bind` \neg ->
                    whitespaces `bind` \_ ->
                      char ',' `bind` \_ ->
                        whitespaces `bind` \_ ->
                          many digit `bind` \zero ->
                            whitespaces `bind` \_ ->
                              char ',' `bind` \_ ->
                                whitespaces `bind` \_ ->
                                  many digit `bind` \pos ->
                                    whitespaces `bind` \_ ->
                                      result (If expr (read neg) (read zero) (read pos))

iff :: Parser Statement
iff = first $ consumeLeadingSpaces if'
