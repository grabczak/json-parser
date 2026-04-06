{-# LANGUAGE OverloadedStrings #-}

module Lib (JValue (..), fromJSON, toJSON) where

import qualified Data.List as L
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
  deriving (Eq)

instance Show JValue where
  show = go 0
   where
    go :: Int -> JValue -> String
    go depth j = case j of
      JNull -> "null"
      JBool True -> "true"
      JBool False -> "false"
      JInt i -> show i
      JDouble d -> show d
      JString s -> show s
      JArray a -> "[\n" <> L.intercalate ",\n" (map (\x -> indent <> go (depth + 1) x) a) <> "\n" <> closeBrace <> "]"
      JObject o -> "{\n" <> L.intercalate ",\n" (map field o) <> "\n" <> closeBrace <> "}"
     where
      indent = replicate ((depth + 1) * 2) ' '
      closeBrace = replicate (depth * 2) ' '
      field (k, v) = indent <> show k <> ": " <> go (depth + 1) v

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
  i <- signed (return ()) decimal
  return $ JInt i

jdouble :: Parser JValue
jdouble = do
  d <- signed (return ()) float
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
fromJSON input = parse (jvalue <* eof) "" (T.pack input)

toJSON :: JValue -> String
toJSON j = case j of
  JNull -> "null"
  JBool True -> "true"
  JBool False -> "false"
  JInt i -> show i
  JDouble d -> show d
  JString s -> "\"" <> s <> "\""
  JArray a -> "[" <> (L.intercalate "," $ map toJSON a) <> "]"
  JObject o -> "{" <> (L.intercalate "," $ map (\(k, v) -> "\"" <> k <> "\"" <> ":" <> toJSON v) o) <> "}"
