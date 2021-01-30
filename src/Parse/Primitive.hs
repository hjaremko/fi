-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Interpreter prostego Fortranu
module Parse.Primitive where

type Parser a = String -> [(a, String)]

-- Zwraca wartosc bez konsumpcji zadnego znaku
result :: a -> Parser a
result v = \input -> [(v, input)]

-- Nie akceptuje zadnego znaku
zero :: Parser a
zero = \input -> []

-- Akceptuje dowolny znak
any' :: Parser Char
any' [] = []
any' (x : xs) = [(x, xs)]

-- Funkcja 'bind'
--
bind :: Parser a -> (a -> Parser b) -> Parser b
-- bind parser f input = concat (map (\(parseResult, rest) -> (f parseResult) rest) (parser input))
-- bind parser f input = concatMap (\(parseResult, rest) -> (f parseResult) rest) (parser input)

bind parser f input = concatMap (uncurry f) (parser input)

-- bind parser f input = (concatMap . uncurry) f (parser input)
-- bind parser f input = flip (concatMap . uncurry) (parser input) f
-- bind parser f input = (flip (concatMap . uncurry) .parser) input f
-- bind parser f input = flip (flip (concatMap . uncurry) .parser) f input
-- bind parser = flip (flip (concatMap . uncurry) . parser)

-- Akceptuje znak spelniajacy predykat
consumeIf :: (Char -> Bool) -> Parser Char
consumeIf predicate =
  any' `bind` \x ->
    if predicate x
      then result x
      else zero

-- Zwraca parser akceptujacy wynik jedngo parsera lub drugiego
or' :: Parser a -> Parser a -> Parser a
or' parser1 parser2 input = parser1 input ++ parser2 input

-- Akceptuje konkretny znak
char :: Char -> Parser Char
char x = consumeIf (x ==)

digit :: Parser Char
digit = consumeIf (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = consumeIf (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = consumeIf (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = lower `or'` upper

alphanum :: Parser Char
alphanum = letter `or'` digit

-- Zwraca parser akceptujacy tak dlugo jak parser podany na wejsciu, wynik moze byc pusty
many :: Parser a -> Parser [a]
many parser = nonEmpty `or'` empty
  where
    nonEmpty = parser `bind` \x -> many parser `bind` \xs -> result (x : xs)
    empty = result []

-- To samo co many, tylko zawsze zawiera co najmniej jeden wynik
manyNotEmpty :: Parser a -> Parser [a]
manyNotEmpty parser =
  parser `bind` \x ->
    many parser `bind` \xs ->
      result (x : xs)

-- Akceptuje konkretny string
string :: String -> Parser String
string "" = result ""
string (x : xs) = char x `bind` \_ -> string xs `bind` \_ -> result (x : xs)

-- Zwraca parser zawierajacy pierwszy wynik z pierwszego parsera,
-- ze wzgledu na niedeterministyczny charakter, czesto interesuje nas tylko
-- pierwszy element na liscie (ten ktory skonsumowal najwiecej znakow)
first :: Parser a -> Parser a
first parser input =
  case parser input of
    [] -> []
    (x : xs) -> [x]

-- Konsumuje biale znaki (moze ich nie byc), ale ich nie zwraca
whitespaces :: Parser ()
whitespaces =
  many (consumeIf isWhitespace) `bind` \x -> result ()
  where
    isWhitespace x = (x == ' ') || (x == '\n') || (x == '\t')

-- Tworzy parser ktory ignoruje biale znaki
consumeLeadingSpaces :: Parser a -> Parser a
consumeLeadingSpaces parser = whitespaces `bind` \_ -> parser `bind` \xs -> result xs
