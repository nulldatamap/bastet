{-# LANGUAGE DeriveFunctor #-}

module Ast
  ( showConstructs, uget, uset, ufield, ucall, unamed, uliteral
  , uif, usequence, toplevelName, ureturn, ucase, ulet, utuple
  , Ident(..), Type(..), Literal(..), Expression(..) , FuncDef(..)
  , AliasDef(..), Construct(..), IfExpr(..), UExpr(..), UFuncDef(..)
  , UConstruct(..), DataDef(..), TypePath(..), Path(..), CaseExpr(..)
  , Pattern(..), LetExpr(..) ) where

import Data.Maybe
import Data.List

import Free

--------------------------------------------------------------------------------
--                                    Utilities                               --
--------------------------------------------------------------------------------

-- Untyped AST:
type UExpr = Free Expression ()
type UConstruct = Construct UExpr
type UFuncDef = FuncDef UExpr

uget f a        = Free $ EGet f a
uset a b        = Free $ ESet a b
ufield f        = Free $ EField f
ucall a b       = Free $ ECall a b
unamed n        = Free $ ENamed n
uliteral litr   = Free $ ELiteral litr
uif a           = Free $ EIf a
ureturn a       = Free $ EReturn a
ucase a         = Free $ ECase a
ulet a          = Free $ ELet a
usequence exprs = Free $ ESequence exprs
utuple a        = Free $ ETuple a

toplevelName    = Path (TypePath [])

--------------------------------------------------------------------------------
--                                 Data structures                            --
--------------------------------------------------------------------------------

type Ident = String

type TypeIdent = String

data TypePath = TypePath [(TypeIdent, [Type])]
  deriving (Eq)

data Path = Path TypePath Ident
  deriving (Eq)

data Type = TNamed TypePath
          | TUnit
          | TFn [Type] Type
          | TParam String
          | TApply Type [Type]
          | TTuple [Type]
          | TUnknown
  deriving (Eq)

data Literal = LInt Integer
             | LChar Char
             | LString String
             | LFloat Double
             | LUnit
  deriving (Eq)

data IfExpr a =
  IfExpr { ifCond :: a
         , ifThen :: a
         , ifElse :: Maybe a }
  deriving (Functor, Eq)

data Pattern = PWildcard
             | PBind Ident
             | PApply TypePath [Pattern]
             | PAt Ident Pattern
             | PValue Literal
             | PTuple [Pattern]
  deriving (Eq)

data CaseExpr a =
  CaseExpr { caseSubject :: a
           , caseCases   :: [(Pattern, a)] }
  deriving (Functor, Eq)

data LetExpr a =
  LetExpr { letBinding :: [(Pattern, a)]
          , letExpr    :: Maybe a }
  deriving (Functor, Eq)

data Expression a = ECall a [a]
                  | ENamed Path
                  | ELiteral Literal
                  | EField Ident
                  | EGet Ident a
                  | ESet a a
                  | EIf (IfExpr a)
                  | EReturn a
                  | ETuple [a]
                  | ECase (CaseExpr a)
                  | ELet (LetExpr a)
                  | ESequence [a]
  deriving (Functor, Eq)

data FuncDef a =
  FuncDef { funcDefName :: Ident
          , funcDefArgs :: [(Ident, Type)]
          , funcDefRet  :: Maybe Type
          , funcDefBody :: Maybe a }
  deriving (Functor, Eq)

-- The list of types represents a sum-type
type TypeConstructor = (Ident, [Type])

-- The list of type constructures represents a product type
data DataDef =
  DataDef { dataDefName :: Ident
          , dataDefArgs :: [Type]
          , dataDefBody :: [TypeConstructor] }
  deriving (Eq)

data AliasDef = 
  AliasDef { aliasDefName :: Ident
           , aliasDefArgs :: [Type]
           , aliasDefBody :: Type }
    deriving (Eq)

data Construct a = CFuncDef (FuncDef a)
                 | CDataDef DataDef
                 | CAliasDef AliasDef
  deriving (Functor, Eq)

--------------------------------------------------------------------------------
--                              Printing functions                            --
--------------------------------------------------------------------------------

pres "" = ""
pres x  = ' ' : x
poss "" = ""
poss x  = x ++ " "

mlace sep [] = ""
mlace sep ts = intercalate sep ts

instance Show TypePath where
  show (TypePath elms) =
    mlace "::" $ map (\(n, aps) ->
      case aps of
        [] -> n 
        _  -> "(" ++ n ++ pres (mlace " " $ map show aps) ++ ")" ) elms

instance Show Path where
  show (Path (TypePath []) n) = n
  show (Path tp n) = show tp ++ "::" ++ n

instance Show Type where
  show (TNamed n) = show n
  show TUnit = "()"
  show TUnknown = "_"
  show (TFn as r) =
    "(fn " ++ mlace " " (map show as) ++ "->" ++ show r ++ ")"
  show (TParam x) = x
  show (TApply f xs) = "(" ++ show f ++ " " ++ mlace " " (map show xs) ++ ")"
  show (TTuple x) = "(" ++ mlace ", " (map show x) ++ ")"

instance Show Literal where
  show (LInt x) = show x
  show LUnit = "()"
  show (LChar x) = "'" ++ x : "'"
  show (LString x) = "\"" ++ x ++ "\""
  show (LFloat x) = show x

instance Show e => Show (FuncDef e) where
  show (FuncDef nm as rt bd) =
    "fn " ++ nm ++ pres argst ++ rett ++ "= " ++ bodyt
    where
      argst = mlace ", " $ map (\(x, t) -> x ++ " : " ++ show t) as
      rett = maybe " " (\x -> " -> " ++ show x ++ " ") rt
      bodyt = maybe "" show bd

instance Show DataDef where
  show (DataDef nm as bd) =
    "data " ++ nm ++ " " ++ argst ++ "= " ++ bodyt
    where
      argst = poss $ mlace " " $ map show as
      bodyt =
        mlace " | "
        $ map (\(ctor, elms) ->
            ctor ++ pres (mlace " " $ map show elms))
          bd

instance Show AliasDef where
  show (AliasDef nm as bd) =
    "alias " ++ nm ++ " " ++ argst ++ "= " ++ show bd
    where
       argst = poss $ mlace " " $ map show as

showConstructs cs = intercalate "\n" $ map show cs

instance Show Pattern where
  show PWildcard = "_"
  show (PBind x) = x
  show (PApply tp as) = show tp ++ pres (mlace " " (map show as))
  show (PValue x) = show x
  show (PAt n e) = n ++ "@" ++ show e
  show (PTuple x) = "(" ++ mlace ", " (map show x) ++ ")"

instance Show e => Show (Construct e) where
  show (CFuncDef x)  = show x
  show (CDataDef x)  = show x
  show (CAliasDef x) = show x

instance Show a => Show (Expression a) where
  show (ESequence elms) =
    intercalate "; " $ map show elms
  show (ECall f args) =
      '(' : show f ++ foldl (\acc x -> acc ++ (' ' : show x)) "" args ++ ")"
  show (ENamed x) = show x
  show (ELiteral x) = show x
  show (ESet x y) = show x ++ " = " ++ show y
  show (EGet x y) = show y ++ "." ++ x
  show (EField x) = "." ++ x
  show (EReturn x) = "return " ++ show x
  show (ECase (CaseExpr x es)) =
    "case " ++ show x ++ " of " ++ l
    where
      l = mlace ", " (map (\(p, e) -> show p ++ " => " ++ show e) es)
  show (EIf (IfExpr c t e)) =
    "if " ++ show c ++ " then " ++ show t ++ maybe "" ((" else " ++) . show) e
  show (ELet (LetExpr ds i)) =
    "let " ++ defs ++ ins
    where
      defs = mlace ", " $ map (\(p, e) -> show p ++ " = " ++ show e) ds
      ins = maybe "" (\x -> " in " ++ show x) i
  show (ETuple x) = "(" ++ mlace ", " (map show x) ++ ")"

