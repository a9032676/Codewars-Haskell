module JSON_Parser (parse) where

import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Char
import Data.Functor(($>))

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null
           deriving Show

parse :: String -> Maybe Value
parse s = either (const Nothing) Just $ runParser (spaces *> valueP <* eof) () "" s

objP, arrayP, valueP :: Parser Value
memsP  :: Parser [(Value, Value)]
pairP  :: Parser (Value, Value)
elemsP :: Parser [Value]

objP   = try (symbol '{' *> (Object <$> memsP) <* symbol '}') <|> (symbol '{' *> symbol '}') $> Object []
memsP  = try (pairP >>= \p -> (:) p <$> (symbol ',' >> memsP)) <|> pure <$> pairP
pairP  = strP >>= \s -> symbol ':' >> valueP >>= \v -> return (String s, v)
arrayP = try (symbol '[' *> (Array <$> elemsP) <* symbol ']') <|> (symbol '[' *> symbol ']') $> Array []
elemsP = try (valueP >>= \v -> (:) v <$> (symbol ',' >> elemsP)) <|> pure <$> valueP
valueP = trimP $
    String  <$> strP  <|>
    Number  <$> numP  <|>
    Boolean <$> boolP <|>
    objP              <|>
    arrayP            <|>
    nullP

strP :: Parser String
strP = between (char '"') (trimP $ char '"') $ many (escape <|> nonEscape)
    where
        escape    = char '\\' *> char '\"'
        nonEscape = noneOf "\""

numP :: Parser Double
numP = read <$> (try (intP <* notFollowedBy (char '.')) <|> (do
    i <- intP
    f <- fracP
    return (i ++ f)))
    where
        intP     = digitP <|> (:) '-' <$> (oneOf "-" *> digitP)
        fracP    = char '.' *> ((:) '.' <$> many1 digit)
        digit1_9 = oneOf ['1'..'9']
        digitP   =
            try (pure <$> char '0' <* notFollowedBy digit) <|>
            (digit1_9 >>= \h -> (:) h <$> many digit)

boolP :: Parser Bool
boolP = string "true" $> True <|> string "false" $> False

nullP :: Parser Value
nullP = string "null" $> Null

symbol :: Char -> Parser Char
symbol = trimP . char

trimP :: Parser a -> Parser a
trimP = flip (<*) spaces
