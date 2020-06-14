module SimpleTokenizer (Token(..), tokenize) where

data Token = Token String | Brackets [Token]
  deriving (Eq, Show)

tokenize :: String -> Maybe [Token]
tokenize l =
  let ((_, pc), res) = tokenize' False 0 [] [] l
  in if pc == 0
    then res
    else Nothing


-- op  :: Is operator
-- pc  :: Parenthesis inside count
-- stk :: Token's stack
-- res :: Result
-- cur :: Current non-process statements

tokenize' op 0  stk  res []       = (([], 0), Just $ to res stk)
tokenize' op pc stk  res []       = (([], pc), Nothing)
tokenize' op pc stk  res (')':xs) = ((xs, pc-1), Just $ to res stk)
tokenize' op pc stk  res (' ':xs) = tokenize' op pc [] (to res stk) xs
tokenize' op pc stk  res ( x :xs)
  | x == '('     =
    let ((xs', pc'), inToks) = tokenize' op (pc+1) [] [] xs
    in
      case inToks of
        Nothing   -> (([], pc'), Nothing)
        Just toks -> tokenize' op pc' [] (
          if null stk
            then res ++ [Brackets toks]
            else res ++ [toTok stk, Brackets toks]
          ) xs'
  | isOp x == op = tokenize' op pc (stk ++ [x]) res xs
  | otherwise    = tokenize' (not op) pc [x] (if null stk then res else res ++ [toTok stk]) xs

to :: [Token] -> String -> [Token]
to res stk = res ++ if null stk then [] else [toTok stk]

toTok :: String -> Token
toTok = Token . foldr (:) []

isOp :: Char -> Bool
isOp = flip elem "!#$%&*+-/<=>@^_.,;"