-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu
module Parse.Io where

import Grammar.Statement
import Parse.Expression
import Parse.Identifier
import Parse.Primitive

print' :: Parser Statement
print' =
  first $
    string "WRITE" `bind` \_ ->
      whitespaces `bind` \_ ->
        printables `bind` \xs ->
          result (Print xs)

printables :: Parser [Printable]
printables =
  ( printable `bind` \p ->
      whitespaces `bind` \_ ->
        char ',' `bind` \_ ->
          whitespaces `bind` \_ ->
            printables `bind` \ps ->
              result (p : ps)
  )
    `or'` ( printable `bind` \p ->
              result [p]
          )

writeString :: Parser Printable
writeString =
  char '\"' `bind` \x ->
    anyButQuotes x `bind` \str ->
      char '\"' `bind` \_ ->
        result (Str str)
  where
    anyButQuotes x = many (consumeIf (/= x))

printable :: Parser Printable
printable =
  first writeString `or'` (first expression `bind` \expr -> result (Expr expr))

write :: Parser Statement
write = first $ consumeLeadingSpaces print'

----------------------------------------------------------

readVar' :: Parser Statement
readVar' =
  string "READ" `bind` \_ ->
    whitespaces `bind` \_ ->
      identifier `bind` \id ->
        result (Read id)

readStr' :: Parser Statement
readStr' =
  string "READ" `bind` \_ ->
    whitespaces `bind` \_ ->
      identifier `bind` \id ->
        result (Read id)

readVar :: Parser Statement
readVar = first $ consumeLeadingSpaces readVar'
