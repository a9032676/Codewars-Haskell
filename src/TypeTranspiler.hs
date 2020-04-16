{-# LANGUAGE FlexibleContexts #-}

module TypeTranspiler where

import Data.Char
import Data.List
import Data.Functor(($>))
import Data.Bifunctor(bimap)
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Text.Parsec hiding (digit)
import Text.Parsec.String

data ParamT   = Any | Covar Type | Cotra Type | TypeP Type          deriving (Show)
data SimUserT = SimUserT Type [ParamT]                              deriving (Show)
data Type     = FuncT [Type] Type | NameT String | UserT [SimUserT] deriving (Show)

alpha = ['_'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']
digit = ['0' .. '9']

transpile :: String -> Either String String
transpile input = bimap (const "Hugh?") converter $ runParser rootP () [] input

converter :: Type -> String
converter = f
    where
        f (FuncT ps r) = foldl (\s t -> s ++ f t ++ ",") ("Function" ++ show (length ps) ++ "<") ps ++ f r ++ ">"
        f (NameT strs) = strs
        f (UserT suts) = foldl (\s t -> s ++ ['.' | not . null $ s] ++ rename (g t)) "" suts
            where
                rename "kotlin" = "java.lang"
                rename "Int"    = "Integer"
                rename "Unit"   = "Void"
                rename n        = n
        
        g (SimUserT t [])  = f t
        g (SimUserT t pts) = f t ++ "<" ++ foldl (\s pt -> s ++ [',' | not . null $ s] ++ h pt) "" pts ++ ">"
        
        h  Any      = "?"
        h (Covar t) = "? extends " ++ f t
        h (Cotra t) = "? super " ++ f t
        h (TypeP t) = f t

nameP :: Parser Type
nameP = trimP $ oneOf alpha >>= \c -> NameT <$> ((c :) <$> many validId)

paramTP :: Parser ParamT
paramTP = trimP $
    char '*'            $>  Any              <|>
    try (string "in ")  *> (Cotra <$> nameP) <|>
    try (string "out ") *> (Covar <$> nameP) <|>
    (TypeP <$> typeP)

simUserTP :: Parser SimUserT
simUserTP = do
    n  <- nameP
    tp <- option [] $ between (trimP . char $ '<') (trimP . char $ '>') $ paramTP `sepBy1` trimP (char ',')
    return $ SimUserT n tp

userP :: Parser Type
userP = UserT <$> simUserTP `sepBy1` trimP (char '.')

paramsP :: Parser [Type]
paramsP = typeP `sepBy` trimP (char ',')

funcTP :: Parser Type
funcTP = do
    ps <- between (char '(') (char ')') $ trimP paramsP
    trimP $ string "->"
    FuncT ps <$> typeP

typeP :: Parser Type
typeP = funcTP <|> userP

rootP :: Parser Type
rootP = trimP typeP <* eof

validId :: Parser Char
validId = oneOf $ alpha ++ digit

trimP :: Parser p -> Parser p
trimP p = spaceP *> p <* spaceP

spaceP :: Parser String
spaceP = many $ char ' '