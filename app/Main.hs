module Main where

import Data.List

import Ast
import Tokenizer
import Parser
import Module

main = do
  src <- readFile "test/test.bt"
  case tokenize src "test.bt" of 
    Right tks -> do
      case parseFromTokens tks "test.bt" of
        Right out ->
          case constructModule out of
            Right mod  -> putStrLn "Compiled succesfully."
            Left  errs -> putStrLn $ intercalate "\n" $ map (\x -> showNameError x src) errs 
        Left err -> do
          putStrLn $ reconstructSpan src tks err
    Left err  -> print $ err
  

