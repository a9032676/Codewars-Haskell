module RegExpParser_PCV
    ( RegExp(..)
    , parseRegExp
    ) where

import Prelude hiding (any, or, seq)

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)

charLiteral :: Parser RegExp
charLiteral = normal <|> any

bracketLiteral :: Parser RegExp
bracketLiteral = char '(' *> seq <* char ')'

normal :: Parser RegExp
normal = Normal <$> noneOf "()*|."

any :: Parser RegExp
any = char '.' *> pure Any

zeroOrMore :: Parser RegExp
zeroOrMore = ZeroOrMore <$> (try bracketLiteral <|> charLiteral) <* char '*'

orExpr :: Parser RegExp
orExpr = try zeroOrMore <|> try bracketLiteral <|> try (Str <$> many (try zeroOrMore <|> try bracketLiteral <|> charLiteral)) <|> charLiteral

or :: Parser RegExp
or = do
  expr' <- orExpr
  char '|'
  Or expr' <$> orExpr

seq :: Parser RegExp
seq = Str <$> many (try or <|> try zeroOrMore <|> try bracketLiteral <|> charLiteral)

expr :: Parser RegExp
expr = try or <|> try zeroOrMore <|> try bracketLiteral <|> try seq <|> charLiteral

parseRegExp :: String -> Maybe RegExp
parseRegExp "" = Nothing
parseRegExp (c:"") =
  case parse charLiteral "" [c] of
    Left  _   -> Nothing
    Right res -> Just res

parseRegExp str =
  case parse expr "" str of
    Left  _   -> Nothing
    Right res -> Just res