module Module (constructModule, showNameError) where

import qualified Data.Map as Map
import Data.List

import Ast
import Span

type PathS = [String]

data Module =
  Module { moduleAliases     :: Map.Map PathS PathS
         , moduleFunctions   :: Map.Map PathS UFuncDef
         , moduleDatatypes   :: Map.Map PathS DataDef
         , moduleTypeAliases :: Map.Map PathS AliasDef }
  deriving (Show)

data AliasOrData = ADAlias AliasDef | ADData DataDef

data NameError =
  FuncAlreadyDefined   PathS UFuncDef    UFuncDef
  | TypeAlreadyDefined PathS AliasOrData AliasOrData

showNameError (FuncAlreadyDefined p ori new) src =
  "Error: function '" ++ pn ++ "' has multiple definitions\n"
  ++ "Occuring at:\n" ++ showSpan new src
  ++ "\n'" ++ pn ++ "' was originally defined at:\n" ++ showSpan ori src 
  where
    pn = (intercalate "::" p) 

constructModule :: [UConstruct] -> Either [NameError] Module
constructModule constr =
  case buildModule of
    (mod, []) -> Right mod
    (_, errs) -> Left  errs
  where
    buildModule =
      foldl (\(mod, errs) x -> registerConstruct mod errs x) (initialModule, []) constr

    initialModule = Module Map.empty Map.empty Map.empty Map.empty

    registerConstruct mod@(Module a f d t) errs (Construct (CFuncDef fn) _) = 
      case insertIfNotPresent f fn of
        Left  err -> (mod, err:errs)
        Right f'  -> (Module a f' d t, errs)

    registerConstruct mod errs _ = (mod, errs)

    insertIfNotPresent m fn =
      case Map.lookup funcName m of
        Just original -> Left  $ FuncAlreadyDefined funcName original fn
        Nothing       -> Right $ Map.insert funcName fn m
      where
        funcName = [ identName $ funcDefName fn ]

