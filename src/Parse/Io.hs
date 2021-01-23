module Parse.Io where
import Parse.Primitive
import Grammar.Grammar
import Parse.Expression
import Parse.Identifier


print' :: Parser Statement 
print' = string "WRITE" `bind` \x ->
         spaces `bind` \y ->
         expression `bind` \xs ->
         result (PrintExpr xs)

printVar' :: Parser Statement 
printVar' = string "WRITE" `bind` \x ->
         spaces `bind` \y ->
         identificator `bind` \xs ->
         result (PrintVar (Ident xs))

write :: Parser Statement 
write = first $ consumeLeadingSpaces printVar' `plus`
                consumeLeadingSpaces print'

readVar' :: Parser Statement 
readVar' = string "READ" `bind` \x ->
         spaces `bind` \y ->
         identificator `bind` \xs ->
         result (Read (Ident xs))

readVar :: Parser Statement 
readVar = first $ consumeLeadingSpaces readVar'
