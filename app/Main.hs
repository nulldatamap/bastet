module Main where

import Ast
import Parser

main = do
  rslt <- parseFromFile "test/test.bt"
  case rslt of 
    Right out -> putStrLn $ show_constructs out
    Left err  -> print $ err
  
