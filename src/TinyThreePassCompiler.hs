module TinyThreePassCompiler where

import Data.Functor(($>))
import Data.List(foldl')
import Text.Parsec hiding (digit)
import Text.Parsec.String
import Text.Parsec.Char hiding (digit)

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 = either (error . show) id . runParser funcP () ""

funcP :: Parser AST
funcP = do 
    args <- between (symbol '[') (symbol ']') argsP
    exprP (zip args [0..])

argsP :: Parser [String]
argsP = varP `sepEndBy` spaces

varP :: Parser String
varP = many1 . oneOf $ alpha

exprP :: [(String, Int)] -> Parser AST
exprP l = trimP $
    try (
        trimP (termP l)
        `chainl1`
        ((symbol '+' $> Add) <|> (symbol '-' $> Sub))
        ) <|>
    termP l

termP :: [(String, Int)] -> Parser AST
termP l = trimP $
    try (
        trimP (factorP l)
        `chainl1`
        ((symbol '*' $> Mul) <|> (symbol '/' $> Div))
        ) <|>
    factorP l

factorP :: [(String, Int)] -> Parser AST
factorP l = Imm . read <$> many1 (oneOf digit) <|>
            maybe (error "Argument not found") Arg . flip lookup l <$> many1 (oneOf alpha) <|>
            between (symbol '(') (symbol ')') (exprP l)

symbol :: Char -> Parser Char
symbol = trimP . char

trimP :: Parser a -> Parser a
trimP = flip (<*) spaces

pass2 :: AST -> AST
pass2 (Add (Imm x) (Imm y)) = Imm (x + y)
pass2 (Sub (Imm x) (Imm y)) = Imm (x - y)
pass2 (Mul (Imm x) (Imm y)) = Imm (x * y)
pass2 (Div (Imm x) (Imm y)) = Imm (x `div` y)
pass2 (Add x y) = reduce Add x y
pass2 (Sub x y) = reduce Sub x y
pass2 (Mul x y) = reduce Mul x y
pass2 (Div x y) = reduce Div x y
pass2 ast = ast

reduce :: (AST -> AST -> AST) -> AST -> AST -> AST
reduce op x y = case pass2 x of
        Imm i -> case pass2 y of
            Imm j -> pass2 $ op (Imm i) (Imm j)
            ast   -> op (Imm i) ast
        ast   -> op ast (pass2 y)


-- Add (Add (Imm 1) (Arg 0)) (Imm 3)
--    []
-- PU [1]
-- PU [x, 1]
-- 

-- [ "IM 1", "PU" ] ++ [ "AR 0", "PU" ] ++ [ "PO", "SW", "PO", "AD" ]

-- (1 + a) + 3 [ "IM 1", "SW", "AR 0", "AD", "SW", "IM 3", "AD"]

-- Add (Sub (Imm 1) (Arg 0)) (Imm 3)
-- (1 - a) + 3 [ "IM 1", "SW", "AR 0", "SU", "SW", "IM 3", "AD"]
pass3 :: AST -> [String]
pass3 (Imm i) = ["IM " ++ show i, "PU"]
pass3 (Arg i) = ["AR " ++ show i, "PU"]
pass3 (Add x y) = swap x y ++ ["AD", "PU"]
pass3 (Sub x y) = swap x y ++ ["SU", "PU"]
pass3 (Mul x y) = swap x y ++ ["MU", "PU"]
pass3 (Div x y) = swap x y ++ ["DI", "PU"]

-- [ 3 7 ]
-- [ x y ] (x + y) / 2

--          Stk        r0    r1
-- AR 0     []         3     0
-- PU       [3]        3     0
-- AR 1     [3]        7     0
-- PU       [7, 3]     7     0
-- PO       [3]        7     0
-- SW       [3]        0     7
-- PO       []         3     7
-- AD       []         10    7
-- PU       [10]       10    7
-- IM 2     [10]       2     7
-- PU       [2, 10]    2     7
-- PO       [10]       2     7
-- SW       [10]       7     2
-- PO       []         10    2
-- DI       []         5     2
-- PU       [5]        5     2

swap :: AST -> AST -> [String]
swap x y = pass3 x ++ pass3 y ++ ["PO", "SW", "PO"]

-- simulate :: [String] -> [Int] -> Int
-- simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
--   step (r0,r1,stack) ins =
--     case ins of
--       ('I':'M':xs) -> (read xs, r1, stack)
--       ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
--       "SW" -> (r1, r0, stack)
--       "PU" -> (r0, r1, r0:stack)
--       "PO" -> (head stack, r1, tail stack)
--       "AD" -> (r0 + r1, r1, stack)
--       "SU" -> (r0 - r1, r1, stack)
--       "MU" -> (r0 * r1, r1, stack)
--       "DI" -> (r0 `div` r1, r1, stack)
--   takeR0 (r0,_,_) = r0