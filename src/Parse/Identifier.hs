module Parse.Identifier where

import Parse.Primitive

identificator :: Parser String
identificator =
  letter `bind` \x ->
    many alphanum `bind` \xs ->
      result (x : xs)

-- identifier' :: Parser String
-- identifier' =
--   letter `bind` \x ->
--     many alphanum `bind` \xs ->
--       result (x : xs)

-- identificator :: Parser String
-- identificator =
--   identifier' `bind` \id ->
--     if not (isKeyword id)
--       then result id
--       else zero

-- isKeyword :: String -> Bool
-- isKeyword = flip elem ["READ", "WRITE", "IF", "GOTO", "DO", "END", "CONTINUE"]