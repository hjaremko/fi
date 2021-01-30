-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Parse.Statement where

import Grammar.Grammar
import Parse.Assignment
import Parse.Expression
import Parse.Goto
import Parse.If
import Parse.Io
import Parse.Primitive

continue :: Parser Statement
continue =
  string "CONTINUE" `bind` \_ ->
    result Continue

do' :: Parser Statement
do' =
  string "DO" `bind` \_ ->
    spaces `bind` \_ ->
      manyNotEmpty digit `bind` \label ->
        spaces `bind` \_ ->
          assignment `bind` \start ->
            char ',' `bind` \_ ->
              spaces `bind` \_ ->
                expression `bind` \stop ->
                  spaces `bind` \_ ->
                    result (LabelStmt (read label) (Loop start stop (FloatLiteral 1)))

stepDo' :: Parser Statement
stepDo' =
  string "DO" `bind` \_ ->
    spaces `bind` \_ ->
      manyNotEmpty digit `bind` \label ->
        spaces `bind` \_ ->
          assignment `bind` \start ->
            char ',' `bind` \_ ->
              spaces `bind` \_ ->
                expression `bind` \stop ->
                  spaces `bind` \_ ->
                    char ',' `bind` \_ ->
                      spaces `bind` \_ ->
                        expression `bind` \step ->
                          result (LabelStmt (read label) (Loop start stop step))

doLoop :: Parser Statement
doLoop = first $ consumeLeadingSpaces $ stepDo' `plus` do'

labelAssignment :: Parser Statement
labelAssignment =
  manyNotEmpty digit
    `bind` \x ->
      spaces `bind` \s ->
        assignment `bind` \st ->
          result (LabelStmt (read x) st)

labelStatement :: Parser Statement
labelStatement =
  first $
    consumeLeadingSpaces
      (many digit)
      `bind` \x ->
        spaces `bind` \s ->
          statement' `bind` \st ->
            result (LabelStmt (read x) st)

statement' :: Parser Statement
statement' =
  doLoop
    `plus` write
    `plus` assignment
    `plus` goto
    `plus` iff
    `plus` readVar
    `plus` continue

statement :: Parser Statement
statement =
  statement'
    `plus` labelStatement
