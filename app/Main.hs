module Main where

import Data.List

import Ast
import Tokenizer
import Parser

main = do
  src <- readFile "test/test.bt"
  case tokenize src "test.bt" of 
    Right tks -> do
      case parseFromTokens tks "test.bt" of
        Right out -> putStrLn $ intercalate "\n" $ map (show) out
        Left err -> print $ err
      -- putStrLn $ showConstructs out
      
    Left err  -> print $ err
  
