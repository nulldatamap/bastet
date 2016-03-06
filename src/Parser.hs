module Parser (parseFromTokens) where

-- Hide span since we use it as a parser
import Prelude hiding (span)

import Span
import Tokenizer
import Ast
import Annotation

import Data.Maybe
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

import Text.Parsec hiding (parseFromFile, satisfy, char, string)
import Text.ParserCombinators.Parsec.Expr

type Parser a = Parsec [Token] () a

parens i = reservedOp "(" *> i <* reservedOp ")"

string     = (\(Token (TString s) _) -> s)   <$> (satisfy isStringToken)
char       = (\(Token (TChar s) _) -> s)     <$> (satisfy isCharToken)
int        = (\(Token (TInt s) _) -> s)      <$> (satisfy isIntToken)
float      = (\(Token (TFloat s) _) -> s)    <$> (satisfy isFloatToken)
operator   = (\(Token (TOperator s) _) -> s) <$> (satisfy isOperatorToken)
ident      = (\(Token (TIdent i) s) -> Ident i s)    <$> (satisfy isIdentToken)
typeIdent  = (\(Token (TTypeIdent t) s) -> TypeIdent t s) <$> (satisfy isTypeIdentToken)

keyword    x = satisfy $ isKeywordToken x
reservedOp x = satisfy $ isReservedOperatorToken x

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos _ (Token _ spn) _ = spanToPos spn
    testTok t = if p t then Just t else Nothing

span :: Parser (Span -> a) -> Parser a
span p = do
  start <- getPosition
  r <- p
  end <- getPosition
  return $ r $ posToSpan start end

getSpan :: Parser a -> Parser (a, Span)
getSpan p = do
  start <- getPosition
  r <- p
  end <- getPosition
  return $ (r, posToSpan start end)

currentSpan :: Parser Span
currentSpan = posToSpan <$> getPosition <*> getPosition

comma = reservedOp ","
semi = reservedOp ";"

opName :: Parser String
opName = reOp <|> operator
  where
    reOp = do
      (Token (TReservedOperator o) _) <- (satisfy anyReOp)
      return o
      where
        anyReOp (Token (TReservedOperator _) _) = True
        anyReOp _                               = False

name = ident <|> parenOp <?> "identifier or operator"
  where
    parenOp = span $ Ident <$> (try (parens opName))

literal :: Parser Literal
literal =
  span $
    ((Literal <$> LInt <$> int) 
    <|> (Literal <$> LFloat <$> float)
    <|> (reservedOp "()" *> (return $ Literal LUnit))
    <|> (Literal <$> LString <$> string)
    <|> (Literal <$> LChar <$> char)
    <?> "literal")

-- A term is the highest precendence element in an expression tree
-- Literals, named values and nested expressions fall under this category
term :: Parser UExpr
term = 
  (span (uliteral <$> literal))
  <|> (parenUExpr)
  <|> (span (unamed <$> path))
  <|> (span (ufield <$> (reservedOp "." *> name)))
  <?> "expression"
  where
    -- Tries to parse an expression inside parens, if there is none parse a unit
    parenUExpr = do
      (elms, s) <- getSpan $ parens (sepBy expressions comma)
      case elms of
        []  -> return $ uliteral (Literal LUnit s) s
        [e] -> return $ e
        rst -> return $ utuple rst s

path = do
  tp <- optionMaybe (typePath <* reservedOp "::")
  id <- name
  return $ Path (fromMaybe (TypePath []) tp) id
  <?> "name path"

-- Tries to parse a chain of field accesors
field :: Parser UExpr
field =
  theField
  where
    theField = do
      t <- term
      fs <- many (spanningFields <$> (reservedOp "." *> name))
      case fs of
        [] -> return t
        _  -> return $ foldl (\acc x -> x acc) t fs
    spanningFields x (Fix y) =
      (uget x (Fix y) (enclosingSpan (exprSpan y) (identSpan x)))

ifExpr =
  span theIf
  where
    theIf = 
      IfExpr <$> (keyword "if" *> expression)
             <*> (keyword "then" *> expression)
             <*> optionMaybe (keyword "else" *> expression)

pattern :: Parser Pattern
pattern =
  wildcard
  <|> unit
  <|> parensPatternOrTuple
  <|> constructor
  <|> span bindOrAt
  <|> (span $ Pattern <$> (PValue <$> literal))
  where
    unit = do
      (_, s) <- getSpan $ reservedOp "()"
      return $ Pattern (PValue (Literal LUnit s)) s
    wildcard = span $ Pattern <$> (reservedOp "_" *> return PWildcard)
    constructor = 
      span $ Pattern <$> (PApply <$> typePath <*> many pattern)
    bindOrAt = do
      n <- ident
      atpat <- optionMaybe $ reservedOp "@" *> pattern
      case atpat of
        Nothing -> return $ Pattern $ PBind n
        Just x  -> return $ Pattern $ PAt n x
    parensPatternOrTuple = do
      (elms, s) <- getSpan $ parens (sepBy pattern comma)
      case elms of
        []  -> return $ Pattern (PValue $ Literal LUnit s) s
        [p] -> return p
        rst -> return $ Pattern (PTuple rst) s

caseExpr =
  span theCase
  where
    theCase =
      CaseExpr <$> (keyword "case" *> expression <* keyword "of")
               <*> sepBy cases comma
    cases = (,) <$> pattern <*> (reservedOp "=>" *> expression)

letDefExpr =
  span theLet
  where
    theLet = 
      LetExpr <$> (keyword "let" *> sepBy1 def comma)
              <*> (optionMaybe $ keyword "in" *> expression)
    
    def = (,) <$> pattern <*> (reservedOp "=" *> expression)

-- appl is the third highest precedence, this groups terms into function 
-- applications ( if any ) and it also parses flow control expressions
appl :: Parser UExpr
appl =
  (span $ uif <$> ifExpr)
  <|> (span $ ureturn <$> (keyword "return" *> expression))
  <|> (span $ ucase <$> caseExpr)
  <|> (span $ ulet <$> letDefExpr)
  <|> callify <$> (getSpan $ many1 field)
  <?> "expression"
  where
    callify ([f], _) = f
    callify ((f:as), s) = ucall f as s

exprTable :: OperatorTable Token () UExpr
exprTable =
  [ [ preOp "-", preKw "not" ]
  , [ leftOp "*", leftOp "/", leftOp "%" ]
  , [ leftOp "+", leftOp "-" ]
  , [ leftOp ">=", leftOp "<=", leftOp ">", leftOp "<" ]
  , [ leftOp "==", leftOp "!=" ]
  , [ leftOp "|", leftOp "&"]
  , [ leftKw "and", leftKw "or" ]
  , [ leftOp' "<|" $ return $ applyThem True
    , leftOp' "|>" $ return $ applyThem False ]
  , [ eqOp ] ]
  where
    applyThem swp ape@(Fix ap) are@(Fix ar) =
      let theSpan = if swp
                    then enclosingSpan (exprSpan ap) (exprSpan ar)
                    else enclosingSpan (exprSpan ar) (exprSpan ap)
      in ucall ape [are] theSpan
    eqOp =
      Infix (reservedOp "=" *> return theOp) AssocNone
      where
        theOp l@(Fix le) r@(Fix re) =
          let theSpan = enclosingSpan (exprSpan le) (exprSpan re)
          in uset l r theSpan

injectNamed f k name = do
  opNm <- span $ unamed <$>
        (toplevelName <$>
          (span $ Ident <$> (k name *> return name)))
  return $ f opNm

leftOp' name f =
  Infix (reservedOp name *> f) AssocLeft

infixOp k a name =
  Infix (injectNamed makeCall k name) a
  where
    makeCall opNm l@(Fix le) r@(Fix re) =
      let theSpan = enclosingSpan (exprSpan le) (exprSpan re)
      in (ucall opNm [l, r]) theSpan

prefixOp k name =
  Prefix (injectNamed makeCall k name)
  where
    makeCall opNm@(Fix opNme) a@(Fix ar) =
      let theSpan = enclosingSpan (exprSpan opNme) (exprSpan ar)
      in (ucall opNm [a]) theSpan

leftOp = infixOp reservedOp AssocLeft
leftKw = infixOp keyword   AssocLeft
preOp = prefixOp reservedOp
preKw = prefixOp keyword

expression :: Parser UExpr
expression = buildExpressionParser exprTable appl

expressions :: Parser UExpr
expressions =
    span $ usequence <$> (sepEndBy1 expression lastUnit semi)
  where
    sepEndBy1 p q sep = (:) <$> p <*> many (sep *> (p <|> q))
    lastUnit =
      span $ (uliteral <$> (Literal LUnit <$> currentSpan))

ptype :: Parser Type
ptype =
  span (tupleType <|> (Type <$> kind))
  where
    kind = 
      (TFn <$> (keyword "fn" *> (sepBy atype comma)) <*> fnret)
      <|> (reservedOp "_" *> return TUnknown)
      <|> (TParam <$> ident)
      <|> (TNamed <$> typePath)
      <|> (reservedOp "()" *> return TUnit)
      <?> "type"

    fnret =
      fromMaybe <$> (span $ return $ Type TUnit)
                <*> optionMaybe (reservedOp "->" *> atype)
    
    tupleType = do
      elms <- parens (sepBy atype comma)
      case elms of
        []  -> return $ Type TUnit
        [t] -> return $ const t
        rst -> return $ Type $ TTuple rst

atype :: Parser Type
atype = do
  (ts, sp) <- getSpan $ many1 ptype
  case ts of
    [t]  -> return $ t
    x:xs -> return $ Type (TApply x xs) sp

typePath = do
  first <- fragment
  rest  <- many $ try (reservedOp "::" *> fragment)
  return $ TypePath $ first : rest
  where
    fragment = nameFragment <|> applyFragment
    nameFragment = (\x -> (x, [])) <$> typeIdent
    applyFragment =
      parens $ (,) <$> typeIdent
                   <*> ((fromMaybe []) <$> (optionMaybe $ many ptype))

funcDef =
  span theFuncDef
  where
    theFuncDef = 
      FuncDef <$> (keyword "fn" *> (name <?> "function name"))
              <*> args
              <*> optionMaybe returnType
              <*> optionMaybe (reservedOp "=" *> expressions <?> "function body")
              <?> "function definition"
    args = sepBy ( (,) <$> ident
                       <*> (reservedOp ":" *> atype
                       <?> "argument type") ) comma
    returnType = reservedOp "->" *> atype <?> "return type"

dataDef =
  span theDataDef
  where
    theDataDef =
      DataDef <$> (keyword "data" *> (typeIdent <?> "data structure name"))
              <*> (many ptype <?> "data structure type parameters")
              <*> (reservedOp "=" *> (typeCtors <?> "data structure body"))
              <?> "data structure definition"

    typeCtors = sepBy1 typeCtor (reservedOp "|")
    typeCtor  = TypeConstructor <$> ((,) <$> typeIdent <*> many ptype)

aliasDef =
  span theAlias
  where
    theAlias =
      AliasDef <$> (keyword "alias" *> (typeIdent <?> "type alias name"))
               <*> (many ptype <?> "type alias type parameters")
               <*> (reservedOp "=" *> atype)
               <?> "type alias"

construct =
  span $ Construct <$> kind
  where
    kind =  (CFuncDef  <$> funcDef)
        <|> (CDataDef  <$> dataDef)
        <|> (CAliasDef <$> aliasDef)


program :: Parser [UConstruct]
program = many construct <* eof

parseFromTokens :: [Token] -> SourceName -> Either ParseError [UConstruct]
parseFromTokens inp nam =
  parse program nam inp
