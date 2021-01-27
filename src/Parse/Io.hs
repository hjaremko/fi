module Parse.Io where

import Grammar.Grammar
import Parse.Expression
import Parse.Identifier
import Parse.Primitive

print' :: Parser Statement
print' =
  first $
    string "WRITE" `bind` \x ->
      spaces `bind` \y ->
        printables `bind` \xs ->
          result (Print xs)

printables :: Parser [Printable]
printables = 
  (
      printable `bind` \p ->
        spaces `bind` \s ->
        char ',' `bind` \c ->
        spaces `bind` \s2 ->
          printables `bind` \ps ->
            result (p:ps)
  )
  `plus` 
  (printable `bind` \p->
  result [p]) 


writeString :: Parser Printable
writeString =
  char '\"' `bind` \x ->
    many (consumeIf (/= x)) `bind` \str ->
      char '\"' `bind` \x ->
        result (Str str)

printable :: Parser Printable
printable =
  -- (first identificator `bind` \id -> result (PVar id))
    -- `plus` 
    first writeString
    `plus` (first expression `bind` \expr -> result (Expr expr))

write :: Parser Statement
write =
  first $
    consumeLeadingSpaces print'

----------------------------------------------------------

readVar' :: Parser Statement
readVar' =
  string "READ" `bind` \x ->
    spaces `bind` \y ->
      identificator `bind` \xs ->
        result (Read xs)

readStr' :: Parser Statement
readStr' =
  string "READ" `bind` \x ->
    spaces `bind` \y ->
      identificator `bind` \xs ->
        result (Read xs)

readVar :: Parser Statement
readVar = first $ consumeLeadingSpaces readVar'
