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
    maybe [] (
      -- filterKeys (colId2Name <$> colIds) .
      maybe id whereF cond .
      joinAll joins .
      table2ResultNames
    ) $ tableOf name
    where
      tableOf = flip lookup database
      lookupV  = lookup . colId2Name

      col2Name n c = concat [n, ".", c]
      colId2Name (ColId n c) = col2Name n c
      cols2Name n = mapKeyR $ col2Name n
      table2ResultNames = mapKeyT $ col2Name name

      whereF (Cond op (ValConst c) (ValConst c')) = filterC $ const $ op c c'
      whereF (Cond op (ValConst c) (ValId col))   = whereF' col $ op c
      whereF (Cond op (ValId col)  (ValConst c))  = whereF' col $ flip op c
      whereF' colId p = filterR $ \(k, v) -> k == colId2Name colId && p v
      
      joinAll :: [Join] -> Table -> Table
      joinAll [] t = t
      joinAll (Join jName (Cond (==) (ValId lcol@(ColId lt lc)) (ValId rcol@(ColId rt rc))) : xs) table = joinAll xs joinT
        where
          joinT | lt == jName && rt == name = [ cols ++ child  cols | cols <- table ]  -- lt is a join table, rt is a current table
                | lt == name && rt == jName = [ cols ++ parent cols | cols <- joinT']  -- rt is a join table, lt is a current table
                | otherwise                 = [ cols ++ child  cols | cols <- table ]  -- lt is a join table, rt is a current table
          joinT' = maybe [] (fmap $ cols2Name jName) $ tableOf jName

          child cols =
            maybe [] (cols2Name lt . head . filterR (\(k, v) -> k == lc && maybe False (v ==) (lookupV rcol cols))) $ tableOf lt
          parent cols =
            head . filterR (\(k, v) -> k == colId2Name lcol && maybe False (v ==) (lookupV rcol cols)) $ table

filterR :: (Column -> Bool) -> Table -> Table
filterR f t = [ cols | cols <- t, col <- cols, f col]

filterC :: (Column -> Bool) -> Table -> Table
filterC f t = [ [col | col <- cols, f col] | cols <- t]

filterKeys :: [String] -> Table -> Table
filterKeys x = filterC $ (`elem` x) . fst

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





db = [ ( "movie"  , [ [ ( "id", "1" ), ( "name", "Avatar"   ), ( "directorID", "1" ) ]
                    , [ ( "id", "2" ), ( "name", "Titanic"  ), ( "directorID", "1" ) ]
                    , [ ( "id", "3" ), ( "name", "Infamous" ), ( "directorID", "2" ) ]
                    , [ ( "id", "4" ), ( "name", "Skyfall"  ), ( "directorID", "3" ) ]
                    , [ ( "id", "5" ), ( "name", "Aliens"   ), ( "directorID", "1" ) ]
                    ]
                  )
                , ( "actor"
                  , [ [ ( "id", "1" ), ( "name", "Leonardo DiCaprio" ) ]
                    , [ ( "id", "2" ), ( "name", "Sigourney Weaver"  ) ]
                    , [ ( "id", "3" ), ( "name", "Daniel Craig"      ) ]
                    ]
                  )
                , ( "director"
                  , [ [ ( "id", "1" ), ( "name", "James Cameron"   ) ]
                    , [ ( "id", "2" ), ( "name", "Douglas McGrath" ) ]
                    , [ ( "id", "3" ), ( "name", "Sam Mendes"      ) ]
                    ]
                  )
                , ( "actor_to_movie"
                  , [ [ ( "movieID", "1" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "2" ), ( "actorID", "1" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "4" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "5" ), ( "actorID", "2" ) ]
                    ]
                  )
                ]