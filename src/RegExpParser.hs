module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Data.List(elemIndices)

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)


parseRegExp :: String -> Maybe RegExp

-- Observable invalid case
parseRegExp "" = Nothing

parseRegExp "*" = Nothing
parseRegExp "(" = Nothing
parseRegExp ")" = Nothing
parseRegExp "|" = Nothing

parseRegExp "." = Just Any
parseRegExp ( x : "") = Just $ Normal x

parseRegExp ('*': _ :"") = Nothing
parseRegExp ('(': _ :"") = Nothing
parseRegExp (')': _ :"") = Nothing
parseRegExp ('|': _ :"") = Nothing

parseRegExp ('.':'*':"") = Just $ ZeroOrMore Any
parseRegExp ( x :'*':"") = Just . ZeroOrMore $ Normal x

-- Brackets denote
parseRegExp s@('(':xs) =
    case rightBracketIndexOf s of
        Nothing  -> Nothing
        (Just i) ->
            case parseRegExp lhs of
                Nothing         -> Nothing
                Just (Str regs) ->
                    case rhs of
                    ""  -> Just $ Str regs
                    "*" -> Just . ZeroOrMore $ Str regs
                    ('*':'|':rhs') -> case parseRegExp rhs' of
                            Nothing -> Nothing
                            Just (Str regs') -> Just $ Or (ZeroOrMore . Str $ regs) (Str regs')
                            Just reg' -> Just $ Or (ZeroOrMore . Str $ regs) reg'
                    ('*':rhs') -> case parseRegExp rhs' of
                            Nothing -> Nothing
                            Just (Str regs') -> Just . Str $ (ZeroOrMore . Str $ regs) : regs'
                            Just reg' -> Just $ Str [ ZeroOrMore . Str $ regs , reg']
                    ('|':rhs') -> case parseRegExp rhs' of
                            Nothing -> Nothing
                            Just (Str regs') -> Just $ Or (Str regs) (Str regs')
                            Just reg' -> Just $ Or (Str regs) reg'
                    _   -> case parseRegExp rhs of
                            Nothing -> Nothing
                            Just (Str regs') -> Just . Str $ regs ++ regs'
                            Just reg' -> Just . Str $ regs ++ [reg']
                Just reg ->
                    case rhs of
                    ""  -> Just reg
                    "*" -> Just $ ZeroOrMore reg
                    ('*':'|':rhs') -> case parseRegExp rhs' of
                            Nothing -> Nothing
                            Just (Str regs') -> Just $ Or (ZeroOrMore reg) (Str regs')
                            Just reg' -> Just $ Or (ZeroOrMore reg) reg'
                    ('|':rhs') -> case parseRegExp rhs' of
                            Nothing -> Nothing
                            Just (Str regs') -> Just $ Or reg (Str regs')
                            Just reg' -> Just $ Or reg reg'
                    ('*':rhs')   -> case parseRegExp rhs' of
                            Nothing -> Nothing
                            Just (Str regs') -> Just . Str $ ZeroOrMore reg : regs'
                            Just reg' -> Just $ Str [ZeroOrMore reg, reg']
                    _   -> case parseRegExp rhs of
                            Nothing -> Nothing
                            Just (Str regs') -> Just . Str $ reg : regs'
                            Just reg' -> Just $ Str [reg, reg']
            where lhs = take (i - 2) xs
                  rhs = drop (i - 1) xs

parseRegExp ('.':'*':xs) =
    case parseRegExp xs of
        Nothing -> Nothing
        Just (Str regs) -> Just . Str $ ZeroOrMore Any : regs
-- Normal case with ZeroOrMore
parseRegExp ( x :'*':xs) =
    case parseRegExp xs of
        Nothing -> Nothing
        Just (Str regs) -> Just . Str $ (ZeroOrMore $ Normal x) : regs
        Just reg        -> Just $ Str [ ZeroOrMore $ Normal x, reg ]

parseRegExp ('.':'|':xs) = case parseRegExp xs of
    Nothing         -> Nothing
    Just (Str regs) -> Just $ Or Any (Str regs)
    Just reg        -> Just $ Or Any reg
parseRegExp ('.':xs) = case parseRegExp xs of
    Nothing         -> Nothing
    Just (Str regs) -> Just . Str $ Any : regs
    Just reg        -> Just $ Str [ Any, reg ]

-- Normal recursive case
parseRegExp ('*':_) = Nothing
parseRegExp (')':_) = Nothing
parseRegExp s@( x :xs) =
    case orDenoteIndexOf s of
        Empty     ->
            case parseRegExp xs of
                Nothing         -> Nothing
                Just (Str regs) -> Just . Str $ Normal x : regs
                Just reg        -> Just $ Str [ Normal x, reg ]
        (Legal i) ->
            case parseRegExp lhs of
                Nothing         -> Nothing
                Just (Str regs) ->
                    case parseRegExp rhs of
                        Nothing -> Nothing
                        Just (Str regs') -> Just $ Or (Str regs) (Str regs')
                        Just reg' -> Just $ Or (Str regs) reg'
                Just reg ->
                    case parseRegExp rhs of
                        Nothing -> Nothing
                        Just (Str regs') -> Just $ Or reg (Str regs')
                        Just reg' -> Just $ Or reg reg'
                where lhs = take i s
                      rhs = drop (i + 1) s
        Illegal   -> Nothing

data LegalState = Empty | Illegal | Legal Int
    deriving (Show, Eq)

orDenoteIndexOf :: String -> LegalState
orDenoteIndexOf = orDenoteIndexOf' 0 0 []
orDenoteIndexOf' _ _ []  "" = Empty
orDenoteIndexOf' _ _ [i] "" = Legal i
orDenoteIndexOf' _ _ _   "" = Illegal
orDenoteIndexOf' n i arr (x:xs)
    | x == '('           = orDenoteIndexOf' (n + 1) (i + 1) arr          xs
    | x == ')'           = orDenoteIndexOf' (n - 1) (i + 1) arr          xs
    | x == '|' && n == 0 = orDenoteIndexOf' n       (i + 1) (arr ++ [i]) xs
    | otherwise          = orDenoteIndexOf' n       (i + 1) arr          xs

bracketIndexR :: Int -> Int -> String -> Maybe Int

-- Input
bracketIndexR 0 0 "" = Nothing
bracketIndexR 0 0 s@(x:xs)
    | not $ validateAmount s = Nothing
    | x == '('               = bracketIndexR 1 1 xs
    | x == ')'               = Nothing
    | otherwise              = bracketIndexR 0 1 xs
    where
        validateAmount = validateAmount' 0 0
        validateAmount' n1 n2 "" = n1 == n2
        validateAmount' n1 n2 (x:xs)
            | x == '('  = validateAmount' (n1 + 1)  n2       xs
            | x == ')'  = validateAmount' n1        (n2 + 1) xs
            | otherwise = validateAmount' n1        n2       xs

-- Processing and output
bracketIndexR 0 i _ = Just i
bracketIndexR n i (x:xs)
    | x == '('  = bracketIndexR (n + 1) (i + 1) xs
    | x == ')'  = bracketIndexR (n - 1) (i + 1) xs
    | otherwise = bracketIndexR  n      (i + 1) xs

rightBracketIndexOf :: String -> Maybe Int
rightBracketIndexOf = bracketIndexR 0 0