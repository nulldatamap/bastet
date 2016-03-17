module Module (constructModule, showNameError) where

import qualified Data.Map as Map
import Data.List
import Debug.Trace

import Tokenizer -- reservedOperators
import Annotation
import Ast
import Span

-- TODO: Add warnings

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

showNameError (UndefinedIdentifier p (Fix e) f) src =
  "Error: undefined identifier '" ++ (intercalate "::" p)
  ++ "' in expression:\n" ++ showSpan e src ++ "\nIn function '"
  ++ (identName $ funcDefName f) ++ "'"

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
  case checkReferences $ checkAliasOverlap
       $ embedLetExprs $ buildModule constr of
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

embedLetExprs :: (Module, [NameError]) -> (Module, [NameError])
embedLetExprs (mod, errs) =
  (embedEachFuncs mod, errs)
  where
    embedEachFuncs (Module a f d t) =
      Module a (Map.map embedFunc f) d t
    
    embedFunc :: UFuncDef -> UFuncDef
    embedFunc f@(FuncDef _ _ _ Nothing _) = f
    embedFunc (FuncDef n a r (Just expr) s) =
      FuncDef n a r (Just $ embedExpr expr) s

    embedExpr :: UExpr -> UExpr
    embedExpr (Fix (Expression expr sp)) =
        Fix $ Expression doIt sp
      where
        doIt = 
          case expr of
            ECall called args      -> ECall (embedExpr called) (map embedExpr args)
            n@(ENamed _)           -> n
            l@(ELiteral _)         -> l
            f@(EField _)           -> f
            EGet f e               -> EGet f (embedExpr e)
            ESet a b               -> ESet (embedExpr a) (embedExpr b)
            EIf (IfExpr c t e s)   ->
              EIf $ IfExpr (embedExpr c) (embedExpr t) (embedExpr `fmap` e) s
            EReturn a              -> EReturn (embedExpr a)
            ETuple a               -> ETuple $ map embedExpr a
            ECase (CaseExpr v c s) ->
              ECase $ CaseExpr (embedExpr v)
                               (map (\(p, e) -> (p, embedExpr e)) c) s
            ELet (LetExpr b e s)   ->
              ELet $ LetExpr (map (\(p, be) ->
                                (p, (embedExpr be))) b)
                             (embedExpr `fmap` e) s
            ESequence seq          -> ESequence $ embedSeq seq

    embedSeq :: [UExpr] -> [UExpr]
    embedSeq [] = []
    embedSeq le@((Fix (Expression (ELet (LetExpr bs Nothing esp)) sp)):[]) = map embedExpr le
    embedSeq ((Fix (Expression (ELet (LetExpr bs Nothing esp)) sp)):rst) =
      [ embedExpr $ ulet (LetExpr bs (Just $ usequence rst seqSpan) esp) sp ]
      where
        seqSpan = enclosingSpan (exprSpan $ unfix $ head rst) (exprSpan $ unfix $ last rst)
    embedSeq (e:rst) = (embedExpr e):(embedSeq rst)

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

data NameContext =
  NameContext [PathS] (Maybe NameContext)
  deriving Show

addToContext :: NameContext -> PathS -> NameContext
addToContext (NameContext ns parent) n =
  NameContext (n : ns) parent

childContexct :: NameContext -> NameContext -> NameContext
childContexct parent (NameContext ns Nothing) =
  NameContext ns $ Just parent
childContexct _ (NameContext _ (Just _)) = error "Context already has child"  

parentContext :: NameContext -> NameContext
parentContext (NameContext _ Nothing) = error "Context has no parent"
parentContext (NameContext _ (Just parent)) = parent

memberOfContext :: PathS -> NameContext -> Bool
memberOfContext elm (NameContext nms mparent) =
  if elm `elem` nms
  then True
  else maybe False (memberOfContext elm) mparent

pathToPathS :: Path -> PathS
pathToPathS (Path (TypePath frags) ident) =
  map (\(tyid, _) -> tyIdentName tyid) frags ++ [(identName ident)]

extractBindings :: Pattern -> [PathS]
extractBindings (Pattern kind _) =
  case kind of
    PBind i     -> [[identName i]]
    PApply _ ps -> fromManyPatterns ps
    PTuple ps   -> fromManyPatterns ps
    _           -> []
  where
    fromManyPatterns ps = foldl (\acc p -> acc ++ extractBindings p) [] ps

checkIdentRefs :: (Module, [NameError]) -> (Module, [NameError])
checkIdentRefs (mod, es) = 
  (mod, Map.fold checkFunctionRefs es funcs)
  where
    initialContext =
      Map.fold (\fn ctx -> addToContext ctx [identName (funcDefName fn)])
               (NameContext (map (:[]) reservedOperators) Nothing) funcs 
    funcs = moduleFunctions mod
    
    checkFunctionRefs :: UFuncDef -> [NameError] -> [NameError]
    checkFunctionRefs fn errs =
      let argumentContext = foldl (\ctx nm -> addToContext ctx [(identName nm)])
                                  (NameContext [] Nothing)
                                  (map fst $ funcDefArgs fn)
          functionContext = childContexct initialContext argumentContext
      in case (funcDefBody fn) of
           Nothing   -> errs
           Just body -> checkExprRefs body functionContext errs fn

    checkExprRefs :: UExpr -> NameContext -> [NameError] -> UFuncDef
                     -> [NameError]
    checkExprRefs (Fix expr) ctx errs fn =
      errsFromCheck ++ errs
      where
        errsFromCheck :: [NameError]
        errsFromCheck =
          case (exprKind expr) of
            ECall called args ->
              foldl (\aerss aexpr -> checkExprRefs aexpr ctx aerss fn)
                    (checkExprRefs called ctx [] fn) args
            ENamed n ->
              if memberOfContext (pathToPathS n) ctx
              then []
              else [(UndefinedIdentifier (pathToPathS n) (Fix expr) fn)]
            EGet _ ge  -> checkExprRefs ge ctx [] fn
            ESet se ve -> checkExprRefs ve ctx (checkExprRefs se ctx [] fn) fn
            EIf (IfExpr ce te ee _) ->
              let ifAndThen = checkExprRefs te ctx (checkExprRefs ce ctx [] fn) fn
              in maybe ifAndThen (\ex -> (checkExprRefs ex ctx ifAndThen fn)) ee
            EReturn re -> checkExprRefs re ctx [] fn
            ETuple te -> foldl (\terrs teex -> checkExprRefs teex ctx terrs fn)
                               [] te
            ECase (CaseExpr subj cas _) ->
              let subjerss = checkExprRefs subj ctx [] fn 
                  caseContext =
                    foldl (\cctx (pat, _) ->
                            foldl (addToContext) cctx $ extractBindings pat)
                          (NameContext [] $ Just ctx) cas
              in foldl (\cerss (_, cexpr) ->
                         checkExprRefs cexpr caseContext cerss fn)
                       subjerss cas
            ELet (LetExpr binds mexpr _) ->
              let letContext =
                    foldl (\lctx (pat, _) ->
                      foldl (addToContext) lctx $ extractBindings pat)
                    (NameContext [] $ Just ctx) binds
                  binderss =
                    foldl (\accerss (_, bexpr) ->
                            checkExprRefs bexpr letContext accerss fn )
                          [] binds
              in case mexpr of
                   Nothing -> binderss
                   Just ex -> checkExprRefs ex letContext binderss fn

            ESequence ses  ->
              foldl (\serrs sqex -> checkExprRefs sqex ctx serrs fn) [] ses
            _        -> []



checkTypeNameRefs (mod, errs) = (mod, errs)