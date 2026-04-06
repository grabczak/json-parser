{-# LANGUAGE OverloadedStrings #-}

module Lib (JValue (..), fromJSON, toJSON) where

import Data.List
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Error = Void
type Input = T.Text
type Parser = Parsec Error Input

data JValue
  = JNull
  | JBool Bool
  | JInt Integer
  | JDouble Double
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq, Show)

jnull :: Parser JValue
jnull = do
  _ <- string "null"
  return JNull

jbool :: Parser JValue
jbool = do
  b <- choice [string "true", string "false"]
  return $ JBool $ b == "true"

jint :: Parser JValue
jint = do
  i <- decimal
  return $ JInt i

jdouble :: Parser JValue
jdouble = do
  d <- float
  return $ JDouble d

jnumber :: Parser JValue
jnumber = try jdouble <|> jint

jstring :: Parser JValue
jstring = do
  _ <- char '"'
  s <- manyTill charLiteral $ char '"'
  return $ JString s

jarray :: Parser JValue
jarray = do
  _ <- char '['
  a <- jvalue `sepBy` (char ',' >> space)
  _ <- char ']'
  return $ JArray a

jpair :: Parser (String, JValue)
jpair = do
  _ <- space
  (JString k) <- jstring
  _ <- space
  _ <- char ':'
  v <- jvalue
  return (k, v)

jobject :: Parser JValue
jobject = do
  _ <- char '{'
  _ <- space
  p <- jpair `sepBy` (char ',')
  _ <- char '}'
  return $ JObject p

jvalue :: Parser JValue
jvalue = do
  _ <- space
  v <-
    choice
      [ jnull
      , jbool
      , jnumber
      , jstring
      , jarray
      , jobject
      ]
  _ <- space
  return v

fromJSON :: String -> Either (ParseErrorBundle Input Error) JValue
fromJSON input = parse jvalue "" (T.pack input)

toJSON :: JValue -> String
toJSON j = case j of
  JNull -> "null"
  JBool b -> show b
  JInt i -> show i
  JDouble d -> show d
  JString s -> s
  JArray a -> intercalate "," $ map toJSON a
  JObject o -> intercalate "," $ map (\(k, v) -> k ++ ":" ++ toJSON v) o
