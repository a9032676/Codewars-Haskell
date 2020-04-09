{-# LANGUAGE LambdaCase #-}
module ApplicativeParser where

import Data.Char
import Data.List (stripPrefix)
import Prelude hiding (fmap, (<>))

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \s -> [ (s', f a) | (s', a) <- unP p s]

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
-- a <# p = P $ \s -> [ (s', a) | (s', b) <- unP p s]
a <# p = const a <#> p

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \case
    []     -> []
    (x:xs) -> [(xs, x) | p x]

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = P $ \case
    []     -> []
    (x:xs) -> [(xs, x) | x == c]

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \s -> [ (s'', ab a) | (s', ab) <- unP pf s, (s'', a) <- unP px s']

(<@) :: Parser a -> Parser b -> Parser a
-- pa <@ pb = P $ \s -> [ (s'', a) | (s', a) <- unP pa s, (s'', _) <- unP pb s']
pa <@ pb = const <#> pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
-- pa @> pb = P $ \s -> [ (s'', b) | (s', _) <- unP pa s, (s'', b) <- unP pb s']
pa @> pb = flip const <#> pa <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP x = P $ \s -> maybe [] (\s' -> [(s', x)]) $ stripPrefix x s

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p <<>> q = P $ \s -> unP p s ++ unP q s

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = P $ \s ->
    case unP (pure <#> p) s of
        []             -> [(s, [])]
        [(s', as)]     -> unP ((as ++) <#> many p) s'
        x@((s', as):_) -> x ++ unP ((as ++) <#> many p) s'


-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = P $ \s ->
    case unP ((:[]) <#> p) s of
        [] -> []
        _  -> unP (many p) s


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = snd <$> unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case unP p cs of
    [("", a)] -> Just a
    _         -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE i)           = i
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (NegE e)             = - evalExpr e
evalExpr  ZeroE               = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

exprP :: Parser Expr
exprP = constP <<>> binOpExprP exprP exprP <<>> negP exprP <<>> zeroP

numP :: Parser Int
numP = read <#> some (foldl (<<>>) emptyP $ charP <$> ['0'..'9'])

constP :: Parser Expr
constP = ConstE <#> numP

binOpExprP :: Parser Expr -> Parser Expr -> Parser Expr
binOpExprP p q = charP '(' @> p <> q <@ charP ')'

(<>) :: Parser Expr -> Parser Expr -> Parser Expr
p <> q = flip BinOpE <#> (p <@ charP ' ') <@> binOpP <@> (charP ' ' @> q)

binOpP :: Parser BinOp
binOpP = const AddBO <#> charP '+' <<>> const MulBO <#> charP '*'

negP :: Parser Expr -> Parser Expr
negP p = charP '-' @> (NegE <#> p)

zeroP :: Parser Expr
zeroP = charP 'z' @> inject ZeroE

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique exprP