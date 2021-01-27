module Parse.Primitive where

type Parser a = String -> [(a, String)]

result :: a -> Parser a
result v = \input -> [(v, input)]

zero :: Parser a
zero = \input -> []

item :: Parser Char
item [] = []
item (x : xs) = [(x, xs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind parser f input = concat (map (\(parseResult, rest) -> (f parseResult) rest) (parser input))
-- bind parser f input = concatMap (uncurry f) (parser input)
-- bind parser f input = (concatMap . uncurry) f (parser input)
-- bind parser f input = flip (concatMap . uncurry) (parser input) f
-- bind parser f input = (flip (concatMap . uncurry) .parser) input f
-- bind parser f input = flip (flip (concatMap . uncurry) .parser) f input
-- bind parser = flip (flip (concatMap . uncurry) . parser)

consumeIf :: (Char -> Bool) -> Parser Char
consumeIf predicate =
  item `bind` \x ->
    if predicate x
      then result x
      else zero

plus :: Parser a -> Parser a -> Parser a
plus p q input = p input ++ q input

-- akceptuje konkretny char
char :: Char -> Parser Char
char x = consumeIf (x ==)

digit :: Parser Char
digit = consumeIf (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = consumeIf (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = consumeIf (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = plus lower upper

alphanum :: Parser Char
alphanum = plus letter digit

many :: Parser a -> Parser [a]
many p = plus nonEmpty empty
  where
    nonEmpty =
      bind
        p
        ( \x ->
            bind
              (many p)
              ( \xs ->
                  result (x : xs)
              )
        )
    empty = result []

manyNotEmpty :: Parser a -> Parser [a]
manyNotEmpty p =
  p `bind` \x ->
    many p `bind` \xs ->
      result (x : xs)

word :: Parser String
word = many letter

-- akceptuje konkretny string
string :: String -> Parser String
string "" = result ""
string (x : xs) =
  bind
    (char x)
    ( \_ ->
        bind
          (string xs)
          ( \_ ->
              result (x : xs)
          )
    )

first :: Parser a -> Parser a
first p input = case p input of
  [] -> []
  (x : xs) -> [x]

spaces :: Parser ()
spaces =
  many (consumeIf isWhitespace) `bind` \x ->
    result ()
  where
    isWhitespace x = (x == ' ') || (x == '\n') || (x == '\t')

consumeLeadingSpaces :: Parser a -> Parser a
consumeLeadingSpaces p =
  spaces `bind` \x ->
    p `bind` \xs ->
      result xs
