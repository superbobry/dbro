{-# LANGUAGE OverloadedStrings, CPP #-}

module Data.Bro.Parser
  ( statement

#ifdef DEBUG
  , projection
  , expr
  , condition
#endif
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), pure)
import Control.Monad (void, when)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S

import Data.Attoparsec.ByteString.Char8 (Parser, Number(..), (<?>), choice,
                                         takeWhile1, sepBy, sepBy1,
                                         number, decimal,
                                         char, stringCI, skipSpace, option)

import Data.Bro.Simple (simplify)
import Data.Bro.Types (TableName, TableSchema,
                       ColumnName, ColumnType(..), ColumnValue(..),
                       Projection(..), Condition(..), Expr(..), Statement(..))

statement :: Parser Statement
statement = choice [selectFrom, createTable, insertInto, update, delete]
            <* skipSpace
            <* char ';'
  where
    createTable = do
        tokens ["create", "table"]
        CreateTable <$> tableName <*> tableSchema

    insertInto = do
        tokens ["insert", "into"]
        table <- tableName
        columns <- listOf1 columnName <|> pure []
        token "values"
        values <- listOf1 columnValue
        return $ InsertInto table columns values

    selectFrom = do
        token "select"
        p <- projection
        token "from"
        table <- tableName
        c <- option Nothing $ token "where" *> (Just <$> condition)
        return $ Select table p c

    update = as "update" $! do
        token "update"
        table <- tableName
        token "set"
        bindings <- (`sepBy1` (char ',' <* skipSpace)) $ do
            name <- columnName
            token "="
            e <- expr
            return (name, e)
        c <- option Nothing $ token "where" *> (Just <$> condition)
        return $ Update table bindings c

    delete = as "delete" $! do
        tokens ["delete", "from"]
        table <- tableName
        c <- option Nothing $ token "where" *> (Just <$> condition)
        return $ Delete table c

expr :: Parser Expr
expr = as "expr" $! simplify <$> compound
  where
    term = choice [ between '(' ')' expr
                  , Const <$> columnValue
                  , Field <$> columnName
                  , token "-" *> (Negate <$> term)
                  ]
    basic = choice [ Multiply <$> term <*> (token "*" *> basic)
                   , Divide <$> term <*> (token "/" *> basic)
                   , term
                   ]
    compound = choice [ Add <$> basic <*> (token "+" *> expr)
                      , Sub <$> basic <*> (token "-" *> expr)
                      , basic
                      ]

projection :: Parser Projection
projection = as "projection" $! do
    exprs <- choice [ token "*" *> pure []
                    , expr `sepBy` (char ',' <* skipSpace)
                    ]
    return $ Projection exprs

condition :: Parser Condition
condition = as "condition" $! simplify <$> do
    field <- columnName
    choice [ token "=" *> (Equals field <$> expr)
           , token "!=" *> (NotEquals field <$> expr)
           , token ">" *> (GreaterThan field <$> expr)
           , token "<" *> (LowerThan field <$> expr)
           , token ">=" *> (GreaterThan `orEquals` field <$> expr)
           , token "<=" *> (LowerThan `orEquals` field <$> expr)
           , token "and" *> (And <$> condition <*> condition)
           , token "or" *> (Or <$> condition <*> condition)
           ]
  where
    orEquals :: (ColumnName -> Expr -> Condition) -> ColumnName -> Expr -> Condition
    orEquals con field expr0 = con field expr0 `Or` Equals field expr0

tableName :: Parser TableName
tableName = word

tableSchema :: Parser TableSchema
tableSchema = spaced . listOf1 $ do
    name <- columnName <* skipSpace
    t    <- columnType
    return (name, t)

columnName :: Parser ColumnName
columnName = word

columnType :: Parser ColumnType
columnType = choice
             [ stringCI "int" *> pure IntegerColumn
             , stringCI "double" *> pure DoubleColumn
             , varcharType
             ]
  where
    varcharType :: Parser ColumnType
    varcharType = VarcharColumn <$> do
        token "varchar"
        -- Note(Sergei): we default 'varchar' to 'varchar(MAX).
        between '(' ')' decimal <|> return maxBound

columnValue :: Parser ColumnValue
columnValue = varcharValue <|> numberValue where
  string :: Parser S.ByteString
  string = takeWhile1 $ \ch -> not (isSpace ch) && ch /= '\'' && ch /= '"'

  varcharValue :: Parser ColumnValue
  varcharValue = VarcharValue <$> (between '"' '"' string <|>
                                   between '\'' '\'' string)

  numberValue :: Parser ColumnValue
  numberValue = number >>= \result ->
      return $ case result of
          I i -> IntegerValue $ fromIntegral i
          D d -> DoubleValue d

spaced :: Parser a -> Parser a
spaced p = skipSpace *> p <* skipSpace
{-# INLINE spaced #-}

word :: Parser S.ByteString
word = do
    name <- takeWhile1 isAlphaNum
    when (Set.member (S.map toLower name) reserved) $
        fail (S.unpack name ++ "is reserved!")
    return name
  where
    reserved :: Set S.ByteString
    reserved = Set.fromList
               ["select", "from", "update", "set", "insert", "into",
                "where"]

{-# INLINE word #-}

token :: S.ByteString -> Parser ()
token = void . spaced . stringCI
{-# INLINE token #-}

tokens :: [S.ByteString] -> Parser ()
tokens = mapM_ token
{-# INLINE tokens #-}

between :: Char -> Char -> Parser a -> Parser a
between open close p = char open *> p <* char close
{-# INLINE between #-}

listOf1 :: Parser a -> Parser [a]
listOf1 p = between '(' ')' . spaced $ p `sepBy1` (char ',' <* skipSpace)
{-# INLINE listOf1 #-}

as :: String -> Parser a -> Parser a
as = flip (<?>)
{-# INLINE as #-}
