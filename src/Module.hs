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
  FuncAlreadyDefined    PathS UFuncDef UFuncDef
  | DataAlreadyDefined  PathS DataDef  DataDef
  | AliasAlreadyDefined PathS AliasDef AliasDef
  | AliasOverlapsData   PathS AliasDef DataDef
  | UndefinedIdentifier PathS UExpr    UFuncDef

showNameError' what p ori new src =
  "Error: " ++ what ++ " '" ++ pn ++ "' has multiple definitions\n"
  ++ "Occuring at:\n" ++ showSpan new src
  ++ "\n'" ++ pn ++ "' was originally defined at:\n" ++ showSpan ori src 
  where
    pn = (intercalate "::" p) 

showNameError (FuncAlreadyDefined p ori new) src =
  showNameError' "function" p ori new src

showNameError (DataAlreadyDefined p ori new) src =
  showNameError' "type" p ori new src

showNameError (AliasAlreadyDefined p ori new) src =
  showNameError' "type alias" p ori new src

showNameError (AliasOverlapsData p al da) src =
  "Error: type alias '" ++ pn ++ "' overlaps with an already existing type\n"
  ++ "Occuring at:\n" ++ showSpan al src
  ++ "\nThe type '" ++ pn ++ "' is defined at:\n" ++ showSpan da src
  where
    pn = intercalate "::" p

constructModule :: [UConstruct] -> Either [NameError] Module
constructModule constr =
  case checkReferences $ checkAliasOverlap $ buildModule constr of
    (mod, []) -> Right mod
    (_, errs) -> Left  errs

buildModule :: [UConstruct] -> (Module, [NameError])
buildModule constr =
  foldl (\(mod, errs) x -> registerConstruct mod errs x)
        (initialModule, []) constr
  where
    initialModule = Module Map.empty Map.empty Map.empty Map.empty

registerConstruct mod@(Module a f d t) errs (Construct (CFuncDef fn) _) = 
  case Map.lookup funcName f of
    Just original -> (mod, (FuncAlreadyDefined funcName original fn):errs)
    Nothing       -> (Module a (Map.insert funcName fn f) d t, errs)
  where
    funcName = [ identName $ funcDefName fn ]

registerConstruct mod@(Module a f d t) errs (Construct (CDataDef da) _) = 
  case Map.lookup dataName d of
    Just original -> (mod, (DataAlreadyDefined dataName original da):errs)
    Nothing       -> (Module a f (Map.insert dataName da d) t, errs)
  where
    dataName = [ tyIdentName $ dataDefName da ]

registerConstruct mod@(Module a f d t) errs (Construct (CAliasDef al) _) = 
  case Map.lookup aliasName t of
    Just original -> (mod, (AliasAlreadyDefined aliasName original al):errs)
    Nothing       -> (Module a f d (Map.insert aliasName al t), errs)
  where
    aliasName = [ tyIdentName $ aliasDefName al ]

checkAliasOverlap :: (Module, [NameError]) -> (Module, [NameError])
checkAliasOverlap (mod, errs) =
  (mod, Map.foldlWithKey checkOverlap errs (moduleTypeAliases mod))
  where
    checkOverlap acc aln al =
      case Map.lookup aln (moduleDatatypes mod) of
        Just ori -> (AliasOverlapsData aln al ori):acc
        Nothing  -> acc

checkReferences :: (Module, [NameError]) -> (Module, [NameError])
checkReferences (mod, errs) =
  checkTypeNameRefs $ checkIdentRefs (mod, errs)

checkIdentRefs (mod, errs) = 
  (mod, errs)

checkTypeNameRefs (mod, errs) = (mod, errs)

