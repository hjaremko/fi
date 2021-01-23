module Parse.Assignment where

import Grammar.Grammar
import Parse.Expression
import Parse.Identifier
import Parse.Primitive

assignment' :: Parser Statement
assignment' =
  identificator `bind` \x ->
    spaces `bind` \z ->
      char '=' `bind` \y ->
        spaces `bind` \z ->
          expression `bind` \xs ->
            result (Assignment (Ident x) xs)

assignment :: Parser Statement
assignment = first $ consumeLeadingSpaces assignment'
