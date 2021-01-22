-- Hubert Jaremko - Programowanie funkcyjne 2019/2020
-- Prosty interpreter Fortranu

module Parsers where
import  Grammar


type Parser a = String -> [(a, String)]

result :: a -> Parser a
result v =  \input -> [(v, input)]

zero :: Parser a
zero = \input -> []


item :: Parser Char 
item = \input -> case input of
    [] -> []
    (x:xs) -> [(x,xs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = \input -> concat [f v input' | (v, input') <- p input]

sequence :: Parser a -> Parser b -> Parser (a, b)
sequence p q = p `bind` \x ->
               q `bind` \y ->
               result (x, y)

consumeIf :: (Char -> Bool) -> Parser Char 
consumeIf predicate = item `bind` \x ->
                        if predicate x
                        then result x
                        else zero

plus :: Parser a -> Parser a -> Parser a
plus p q = \input -> (p input ++ q input)

-- akceptuje konkretny char
char :: Char -> Parser Char
char x = consumeIf (\y -> x == y)

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
    where nonEmpty = bind p (\x ->
                     bind (many p) (\xs -> 
                     result (x:xs)))
          empty = result []

word :: Parser String
word = many letter

-- akceptuje konkretny string
string :: String -> Parser String 
string "" = result ""
string (x: xs) = bind (char x) (\_ ->
                 bind (string xs) (\_ ->
                 result (x:xs)))

identificator :: Parser String 
identificator = letter `bind` \x ->
                many alphanum `bind` \xs ->
                result (x:xs)

first :: Parser a -> Parser a
first p = \input -> case p input of
                    [] -> []
                    (x:xs) -> [x]

spaces :: Parser ()
spaces = many (consumeIf isWhitespace) `bind` \x ->
                                        result ()
        where isWhitespace x = (x == ' ') || (x == '\n') || (x == '\t') 

-- todo: ujemne
float :: Parser Expr
float = many digit `bind` \x ->
        char '.' `bind` \y ->
        many digit `bind` \xs ->
        result (FloatLiteral (read (x++[y]++xs)))

int :: Parser Expr 
int = many digit `bind` \x ->
        result (FloatLiteral (read x))

expression :: Parser Expr
expression = float `plus` int `plus`
    result (FloatLiteral 0)

assignment' :: Parser Statement
assignment' = identificator `bind` \x ->
             spaces `bind` \z ->
             char '=' `bind` \y ->
             spaces `bind` \z ->
             expression `bind` \xs ->
             result (Assignment (Ident x) xs)

consumeLeadingSpaces :: Parser a -> Parser a
consumeLeadingSpaces p = spaces `bind` \x ->
                         p `bind` \xs ->
                         result xs

assignment :: Parser Statement
assignment = first $ consumeLeadingSpaces assignment'

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

do' :: Parser Statement 
do' = string "DO" `bind` \x ->
      spaces `bind` \y -> 
      assignment `bind` \start ->
      char ',' `bind` \q ->
      spaces `bind` \w -> 
      expression `bind` \stop ->
      spaces `bind` \s -> 
      many statement `bind` \stmts ->
    --   statement `bind` \stmts ->
      spaces `bind` \s -> 
      string "END DO" `bind` \xs ->
      result (Loop start stop (FloatLiteral 1) stmts)
    --   result (Loop start stop [stmts])
    --   result (Loop start stop [])
    --   result (Loop (Assignment (Ident "aa") (FloatLiteral 1)) (FloatLiteral 1) [])
      
stepDo' :: Parser Statement 
stepDo' = string "DO" `bind` \x ->
      spaces `bind` \y -> 
      assignment `bind` \start ->
      char ',' `bind` \q ->
      spaces `bind` \w -> 
      expression `bind` \stop ->
      char ',' `bind` \q ->
      spaces `bind` \s -> 
      expression `bind` \step ->
      many statement `bind` \stmts ->
      spaces `bind` \s -> 
      string "END DO" `bind` \xs ->
      result (Loop start stop step stmts)

doLoop :: Parser Statement
doLoop = first $ consumeLeadingSpaces $ do' `plus` stepDo' 

labelStatement :: Parser Statement 
labelStatement = first $ consumeLeadingSpaces 
                 (many digit) `bind` \x ->
                 spaces `bind` \s ->
                 statement' `bind` \st ->
                 result (LabelStmt (read x) st)


statement' :: Parser Statement
statement' = doLoop 
            `plus` readVar 
            `plus` readVar 
            `plus` write 
            `plus` assignment 

statement :: Parser Statement
statement =  statement'
            `plus` labelStatement