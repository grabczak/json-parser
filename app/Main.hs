module Main (main) where

import Lib (fromJSON, toJSON)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  print "Path to JSON file:"
  path <- getLine
  contents <- readFile path
  print "File contents:"
  print contents
  case fromJSON contents of
    Left err -> print $ "Error parsing JSON: " ++ errorBundlePretty err
    Right jvalue -> do
      print "Parsed JSON value:"
      print jvalue
      print "Stringified back to JSON:"
      print $ toJSON jvalue
