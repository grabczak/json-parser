module Main (main) where

import Lib
import Text.Megaparsec

main :: IO ()
main = do
  putStr "Path to JSON: "
  path <- getLine
  contents <- readFile path
  putStrLn ""
  case fromJSON contents of
    Left err -> do
      putStrLn "Parse failed:"
      putStrLn $ errorBundlePretty err
    Right jvalue -> do
      putStrLn "Parsed JSON:"
      putStrLn $ show jvalue
      putStrLn ""
      putStrLn "Re-encoded JSON:"
      putStrLn $ toJSON jvalue
