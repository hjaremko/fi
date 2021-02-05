-- Hubert Jaremko - Functional programming 2020/2021
-- Simple Fortran interpreter

module Parse.Statement where

import Grammar.Statement
import Parse.Assignment
import Parse.Expression
import Parse.Goto
import Parse.If
import Parse.Io
import Parse.Loop
import Parse.Primitive

labelStatement :: Parser Statement
labelStatement =
  first $
    consumeLeadingSpaces
      (many digit)
      `bind` \label ->
        whitespaces `bind` \_ ->
          statement' `bind` \st ->
            result (LabelStmt (read label) st)

statement' :: Parser Statement
statement' =
  doLoop
    `or'` write
    `or'` assignment
    `or'` goto
    `or'` iff
    `or'` readVar
    `or'` continue

statement :: Parser Statement
statement = statement' `or'` labelStatement
