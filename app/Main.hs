{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import OurScheme.Repl

main :: IO ()
main = do
  T.putStrLn "Let's repl!"
  ret <- repl
  case ret of
    Left err -> T.putStrLn $ "Oh no, " <> err
    Right () -> pure ()
