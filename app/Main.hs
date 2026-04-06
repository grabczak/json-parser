module Main (main) where

import Lib
import Text.Megaparsec

main :: IO ()
main = do
  putStrLn "Path to JSON file:"
  let path = "./example.json"
  contents <- readFile path
  putStrLn "File contents:"
  putStrLn contents
  case fromJSON contents of
    Left err -> putStrLn $ errorBundlePretty err
    Right jvalue -> do
      putStrLn "Parsed JSON value:"
      putStrLn $ show jvalue
