{-# LANGUAGE DeriveFunctor #-}

module Ast
  ( showConstructs, uget, uset, ufield, ucall, unamed, uliteral
  , uif, usequence, toplevelName, ureturn, ucase, ulet, utuple, posToSpan
  , Ident(..), Type(..), Literal(..), Expression(..) , FuncDef(..)
  , AliasDef(..), Construct(..), IfExpr(..), UExpr(..), UFuncDef(..)
  , UConstruct(..), DataDef(..), TypePath(..), Path(..), CaseExpr(..)
  , Pattern(..), LetExpr(..), TypeKind(..), LiteralKind(..), ConstructKind(..)
  , ExpressionKind(..), PatternKind(..), Span(..), TypeIdent(..)
  , TypeConstructor(..), enclosingSpan, Spanable(..) ) where

import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

import Span
import Annotation

--------------------------------------------------------------------------------
--                                    Utilities                               --
--------------------------------------------------------------------------------

type AnnExpr i = Fix (Ann i Expression)

-- Untyped AST:
type UExpr = Fix Expression
type UConstruct = Construct UExpr
type UFuncDef = FuncDef UExpr

uget f a s        = Fix $ Expression (EGet f a) s
uset a b s        = Fix $ Expression (ESet a b) s
ufield f s        = Fix $ Expression (EField f) s
ucall a b s       = Fix $ Expression (ECall a b) s
unamed n s        = Fix $ Expression (ENamed n) s
uliteral litr s   = Fix $ Expression (ELiteral litr) s
uif a s           = Fix $ Expression (EIf a) s
ureturn a s       = Fix $ Expression (EReturn a) s
ucase a s         = Fix $ Expression (ECase a) s
ulet a s          = Fix $ Expression (ELet a) s
usequence exprs s = Fix $ Expression (ESequence exprs) s
utuple a s        = Fix $ Expression (ETuple a) s

toplevelName p  = Path (TypePath []) p

--------------------------------------------------------------------------------
--                                 Data structures                            --
--------------------------------------------------------------------------------

data Ident =
  Ident { identName :: String
        , identSpan  :: Span }
  deriving (Eq)

data TypeIdent =
  TypeIdent { tyIdentName :: String
            , tyIdentSpan  :: Span }
  deriving (Eq)

data TypePath = TypePath [(TypeIdent, [Type])]
  deriving (Eq)

data Path =
  Path { pathFragments  :: TypePath
       , pathIdent      :: Ident }
  deriving (Eq)

data TypeKind =
  TNamed TypePath
  | TUnit
  | TFn [Type] Type
  | TParam Ident
  | TApply Type [Type]
  | TTuple [Type]
  | TUnknown
  deriving (Eq)

data Type =
  Type { typeKind :: TypeKind
       , typeSpan  :: Span }
  deriving (Eq)

data LiteralKind =
  LInt Integer
  | LChar Char
  | LString String
  | LFloat Double
  | LUnit
  deriving (Eq)

data Literal =
  Literal { literalKind :: LiteralKind
          , literalSpan  :: Span }
  deriving (Eq)

data IfExpr a =
  IfExpr { ifCond :: a
         , ifThen :: a
         , ifElse :: Maybe a
         , ifSpan  :: Span }
  deriving (Show, Functor, Eq)

data PatternKind =
  PWildcard
  | PBind Ident
  | PApply TypePath [Pattern]
  | PAt Ident Pattern
  | PValue Literal
  | PTuple [Pattern]
  deriving (Eq)

data Pattern =
  Pattern { patternKind :: PatternKind
          , patternSpan  :: Span }
  deriving (Eq)

data CaseExpr a =
  CaseExpr { caseSubject :: a
           , caseCases   :: [(Pattern, a)]
           , caseSpan     :: Span }
  deriving (Show, Functor, Eq)

data LetExpr a =
  LetExpr { letBinding :: [(Pattern, a)]
          , letExpr    :: Maybe a
          , letSpan     :: Span }
  deriving (Show, Functor, Eq)

data ExpressionKind a =
  ECall a [a]
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

data Expression a =
  Expression { exprKind :: ExpressionKind a
             , exprSpan  :: Span }
  deriving (Functor, Eq)

data FuncDef a =
  FuncDef { funcDefName :: Ident
          , funcDefArgs :: [(Ident, Type)]
          , funcDefRet  :: Maybe Type
          , funcDefBody :: Maybe a
          , funcDefSpan  :: Span }
  deriving (Functor, Eq)

-- The list of types represents a sum-type
data TypeConstructor = TypeConstructor (TypeIdent, [Type])
  deriving (Eq)

-- The list of type constructures represents a product type
data DataDef =
  DataDef { dataDefName :: TypeIdent
          , dataDefArgs :: [Type]
          , dataDefBody :: [TypeConstructor]
          , dataDefSpan  :: Span }
  deriving (Eq)

data AliasDef = 
  AliasDef { aliasDefName :: TypeIdent
           , aliasDefArgs :: [Type]
           , aliasDefBody :: Type
           , aliasDefSpan  :: Span }
    deriving (Eq)

data ConstructKind a =
  CFuncDef (FuncDef a)
  | CDataDef DataDef
  | CAliasDef AliasDef
  deriving (Functor, Eq)

data Construct a =
    Construct { constructKind :: ConstructKind a
              , constructSpan  :: Span }
  deriving (Functor, Eq)

--------------------------------------------------------------------------------
--                              Printing functions                            --
--------------------------------------------------------------------------------

instance Spanable Ident where
  spanOf = identSpan

instance Spanable TypeIdent where
  spanOf = tyIdentSpan

instance Spanable Type where
  spanOf = typeSpan

instance Spanable Literal where
  spanOf = literalSpan

instance Show a => Spanable (IfExpr a) where
  spanOf = ifSpan

instance Spanable Pattern where
  spanOf = patternSpan

instance Show a => Spanable (CaseExpr a) where
  spanOf = caseSpan

instance Show a => Spanable (LetExpr a) where
  spanOf = letSpan

instance Show a => Spanable (Expression a) where
  spanOf = exprSpan

instance Show a => Spanable (FuncDef a) where
  spanOf = funcDefSpan

instance Spanable DataDef where
  spanOf = dataDefSpan

instance Spanable AliasDef  where
  spanOf = aliasDefSpan

instance Show a => Spanable (Construct a) where
  spanOf = constructSpan

pres "" = ""
pres x  = ' ' : x
spans "" = ""
spans x  = x ++ " "

mlace sep [] = ""
mlace sep ts = intercalate sep ts

instance Show Ident where
  show (Ident v _) = v

instance Show TypeIdent where
  show (TypeIdent v _) = v

instance Show TypePath where
  show (TypePath elms) =
    mlace "::" $ map (\(n, aps) ->
      case aps of
        [] -> show n 
        _  -> "(" ++ show n ++ pres (mlace " " $ map show aps) ++ ")" ) elms

instance Show Path where
  show (Path (TypePath []) n) = show n
  show (Path tp n) = show tp ++ "::" ++ show n

instance Show Type where
  show (Type k _) = show k

instance Show TypeKind where
  show (TNamed n) = show n
  show TUnit = "()"
  show TUnknown = "_"
  show (TFn as r) =
    "(fn " ++ mlace " " (map show as) ++ "->" ++ show r ++ ")"
  show (TParam x) = show x
  show (TApply f xs) = "(" ++ show f ++ " " ++ mlace " " (map show xs) ++ ")"
  show (TTuple x) = "(" ++ mlace ", " (map show x) ++ ")"

instance Show Literal where
  show (Literal k _) = show k

instance Show LiteralKind where
  show (LInt x) = show x
  show LUnit = "()"
  show (LChar x) = "'" ++ x : "'"
  show (LString x) = "\"" ++ x ++ "\""
  show (LFloat x) = show x

instance Show e => Show (FuncDef e) where
  show (FuncDef nm as rt bd _) =
    "fn " ++ show nm ++ pres argst ++ rett ++ "= " ++ bodyt
    where
      argst = mlace ", " $ map (\(x, t) -> show x ++ " : " ++ show t) as
      rett = maybe " " (\x -> " -> " ++ show x ++ " ") rt
      bodyt = maybe "" show bd

instance Show DataDef where
  show (DataDef nm as bd _) =
    "data " ++ show nm ++ " " ++ argst ++ "= " ++ bodyt
    where
      argst = spans $ mlace " " $ map show as
      bodyt =
        mlace " | "
        $ map (\(TypeConstructor (ctor, elms)) ->
            show ctor ++ pres (mlace " " $ map show elms))
          bd

instance Show AliasDef where
  show (AliasDef nm as bd _) =
    "alias " ++ show nm ++ " " ++ argst ++ "= " ++ show bd
    where
       argst = spans $ mlace " " $ map show as

showConstructs cs = intercalate "\n" $ map show cs

instance Show Pattern where
  show (Pattern k _) = show k

instance Show PatternKind where
  show PWildcard = "_"
  show (PBind x) = show x
  show (PApply tp as) = show tp ++ pres (mlace " " (map show as))
  show (PValue x) = show x
  show (PAt n e) = show n ++ "@" ++ show e
  show (PTuple x) = "(" ++ mlace ", " (map show x) ++ ")"

instance Show e => Show (Construct e) where
  show (Construct v _) = show v

instance Show e => Show (ConstructKind e) where
  show (CFuncDef x)  = show x
  show (CDataDef x)  = show x
  show (CAliasDef x) = show x

instance Show a => Show (Expression a) where
  show (Expression k _) = show k

instance Show a => Show (ExpressionKind a) where
  show (ESequence elms) =
    intercalate "; " $ map show elms
  show (ECall f args) =
      '(' : show f ++ foldl (\acc x -> acc ++ (' ' : show x)) "" args ++ ")"
  show (ENamed x) = show x
  show (ELiteral x) = show x
  show (ESet x y) = show x ++ " = " ++ show y
  show (EGet x y) = show y ++ "." ++ show x
  show (EField x) = "." ++ show x
  show (EReturn x) = "return " ++ show x
  show (ECase (CaseExpr x es _)) =
    "case " ++ show x ++ " of " ++ l
    where
      l = mlace ", " (map (\(p, e) -> show p ++ " => " ++ show e) es)
  show (EIf (IfExpr c t e _)) =
    "if " ++ show c ++ " then " ++ show t ++ maybe "" ((" else " ++) . show) e
  show (ELet (LetExpr ds i _)) =
    "let " ++ defs ++ ins
    where
      defs = mlace ", " $ map (\(p, e) -> show p ++ " = " ++ show e) ds
      ins = maybe "" (\x -> " in " ++ show x) i
  show (ETuple x) = "(" ++ mlace ", " (map show x) ++ ")"

