module SimpleSQLEngine (sqlEngine) where

import Prelude hiding (EQ, GT, LT)
import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec hiding (Column)
import Text.Parsec.String

type Table    = [Columns]
type Columns  = [Column]
type Column   = (String, String)
type DataBase = [(String, Table)]

data Query  = Query [ColId] String [Join] (Maybe Cond)
data Join   = Join String Cond
data Cond   = Cond (String -> String -> Bool) Value Value
data ColId  = ColId String String
data Value  = ValId ColId | ValConst String

sqlEngine :: DataBase -> String -> Table
sqlEngine database = execute where
  execute query = either (const []) execQuery $ runParser queryP () "" query

  execQuery :: Query -> Table
  execQuery (Query colIds name joins cond) =
    maybe [] (selectT (colId2Name <$> colIds) . maybe id whereF cond . joinAll joins) $ tableOf name
    where
      tableOf n = toFullName n <$> lookup n database
      lookupR  = lookup . colId2Name

      col2Name n c = concat [n, ".", c]
      colId2Name (ColId n c) = col2Name n c
      cols2Name n = mapKeyR $ col2Name n
      toFullName n = mapKeyT $ col2Name n

      selectT sels t = [ selectR sels cols | cols <- t]
      selectR sels row = foldl (\ res x -> res ++ [col | col <- row, fst col == x]) [] sels
      
      whereF (Cond op (ValConst c) (ValConst c')) = filterC $ const $ op c c'
      whereF (Cond op (ValConst c) (ValId col))   = whereF' col $ op c
      whereF (Cond op (ValId col)  (ValConst c))  = whereF' col $ flip op c
      whereF (Cond op (ValId col)  (ValId col'))  =
        fmap $ \row ->
          maybe [] (\s ->
            maybe [] (\s' ->
              if op s s' then row else []) (lookupR col' row)) $ lookupR col row
      whereF' colId p = filterR $ \(k, v) -> k == colId2Name colId && p v

      joinAll :: [Join] -> Table -> Table
      joinAll [] t = t
      joinAll (Join join (Cond op lv@(ValId lcol) rv@(ValId rcol)) : xs) table = joinAll xs $ findT [] table
        where
          findT :: Table -> Table -> Table
          findT = foldl
              (\ t r ->
                 t ++
                   maybe (maybe []
                   (findR lv r) $ lookupR rcol r)
                   (findR rv r)  (lookupR lcol r))
          findR :: Value -> Columns -> String -> Table
          findR valId row con = fmap (row ++) $ maybe [] (whereF (Cond op (ValConst con) valId)) $ tableOf join

filterR :: (Column -> Bool) -> Table -> Table
filterR f t = [ cols | cols <- t, col <- cols, f col]

filterC :: (Column -> Bool) -> Table -> Table
filterC f t = [ [col | col <- cols, f col] | cols <- t]

mapKeyR :: (String -> String) -> Columns -> Columns
mapKeyR f cols = [ (f k, v) | (k, v) <- cols ]

mapKeyT :: (String -> String) -> Table -> Table
mapKeyT f t = [ mapKeyR f cols | cols <- t]

queryP  = Query <$> (manySpaceP *> selectP) <*> (someSpaceP *> fromP) <*> (joinP `sepEndBy` try someSpaceP) <*> optionMaybe whereP
selectP = skipP "select" *> someSpaceP *> (columnIdP `sepBy1` try (trimManyP $ string ","))
fromP   = skipP "from"   *> someSpaceP *> nameP <* manySpaceP
joinP   = skipP "join"   *> someSpaceP *> (Join <$> nameP <* trimManyP (skipP "on") <*> conditionP)
whereP  = skipP "where"  *> someSpaceP *> conditionP

conditionP :: Parser Cond
conditionP = do
  v <- valueP
  o <- operP
  Cond o v <$> valueP

columnIdP :: Parser ColId
columnIdP = do
  n <- nameP
  char '.'
  ColId n <$> nameP

nameP :: Parser String
nameP = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

valueP :: Parser Value
valueP = (ValId <$> columnIdP) <|> (ValConst <$> constP)

operTokenP :: Parser (String -> String -> Bool)
operTokenP =
  let sym = try . string in
  (>=) <$ sym ">=" <|>
  (<=) <$ sym "<=" <|>
  (/=) <$ sym "<>" <|>
  (==) <$ sym "="  <|>
  (>)  <$ sym ">"  <|>
  (<)  <$ sym "<"

operP :: Parser (String -> String -> Bool)
operP = trimManyP operTokenP

constP :: Parser String
constP = char '\'' *> manyTill ((char '\'' *> anyChar) <|> anyChar) (try $ char '\'' *> notFollowedBy (char '\''))

someSpaceP :: Parser String
someSpaceP = many1 $ oneOf " \n"

manySpaceP :: Parser String
manySpaceP = many $ oneOf " \n"

trimManyP :: Parser a -> Parser a
trimManyP p = manySpaceP *> p <* manySpaceP

skipP :: String -> Parser ()
skipP ""     = return ()
skipP (x:xs) = (char (toLower x) <|> char (toUpper x)) *> skipP xs
