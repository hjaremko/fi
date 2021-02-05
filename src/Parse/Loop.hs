-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Parse.Loop where

import Grammar.Statement
import Parse.Assignment
import Parse.Expression
import Parse.Primitive

continue :: Parser Statement
continue =
  string "CONTINUE" `bind` \_ ->
    result Continue

do' :: Parser Statement
do' =
  string "DO" `bind` \_ ->
    whitespaces `bind` \_ ->
      manyNotEmpty digit `bind` \label ->
        whitespaces `bind` \_ ->
          assignment `bind` \start ->
            char ',' `bind` \_ ->
              whitespaces `bind` \_ ->
                expression `bind` \stop ->
                  whitespaces `bind` \_ ->
                    result (LabelStmt (read label) (Loop start stop (FloatLiteral 1)))

stepDo' :: Parser Statement
stepDo' =
  string "DO" `bind` \_ ->
    whitespaces `bind` \_ ->
      manyNotEmpty digit `bind` \label ->
        whitespaces `bind` \_ ->
          assignment `bind` \start ->
            char ',' `bind` \_ ->
              whitespaces `bind` \_ ->
                expression `bind` \stop ->
                  whitespaces `bind` \_ ->
                    char ',' `bind` \_ ->
                      whitespaces `bind` \_ ->
                        expression `bind` \step ->
                          result (LabelStmt (read label) (Loop start stop step))

doLoop :: Parser Statement
doLoop = first $ consumeLeadingSpaces $ stepDo' `or'` do'
