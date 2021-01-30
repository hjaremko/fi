-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu

module Parse.Statement where

import Grammar.Grammar
import Parse.Assignment
import Parse.If
import Parse.Io
import Parse.Primitive
import Parse.Expression
import Parse.Goto

continue :: Parser Statement 
continue = 
              string "CONTINUE" `bind` \_ ->
                result Continue

do' :: Parser Statement 
do' = string "DO" `bind` \_ ->
      spaces `bind` \_ -> 
      manyNotEmpty digit `bind` \label ->
      spaces `bind` \_ -> 
      assignment `bind` \start ->
      char ',' `bind` \_ ->
      spaces `bind` \_ -> 
      expression `bind` \stop ->
      spaces `bind` \s -> 
      result (LabelStmt (read label) (Loop start stop (FloatLiteral 1)))
      
-- stepDo' :: Parser Statement 
-- stepDo' = string "DO" `bind` \x ->
--       spaces `bind` \y -> 
--       assignment `plus` labelAssignment  `bind` \start ->
--       char ',' `bind` \q ->
--       spaces `bind` \w -> 
--       expression `bind` \stop ->
--       char ',' `bind` \q ->
--       spaces `bind` \s -> 
--       expression `bind` \step ->
--       many statement `bind` \stmts ->
--       spaces `bind` \s -> 
--       string "END DO" `bind` \xs ->
--       result (Loop start stop step (stmts ++ [End]))

doLoop :: Parser Statement
doLoop = first $ consumeLeadingSpaces $ do' --`plus` stepDo' 


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
