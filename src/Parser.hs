module Parser (parseFromString, parseFromFile) where

import Ast
import Free

import Data.Maybe
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

import Text.ParserCombinators.Parsec hiding (parseFromFile)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token 
import Text.ParserCombinators.Parsec.Language


lexerStyle :: Token.LanguageDef ()
lexerStyle = Token.LanguageDef
  { Token.commentStart   = "/*"
  , Token.commentEnd     = "*/"
  , Token.commentLine    = "//"
  , Token.nestedComments = True
  , Token.identStart     = lower
  , Token.identLetter    = alphaNum <|> oneOf "_"
  , Token.opStart        = Token.opLetter lexerStyle
  , Token.opLetter       = oneOf "`~!@$%^&*-+|=;:<>./?"
  , Token.reservedOpNames= [ ":", "*", "/", "%", "+", "-", "=", "&", "|", "!"
                           , "==", "<", ">", "!=", ">=", "<=", "<|", "|>", "->"
                           , "()", "_", ".", "::" ]
  , Token.reservedNames  = [ "and", "or", "not", "if", "then", "else", "fn"
                           , "data", "alias", "return", "where", "of", "in"
                           , "instance", "trait", "impl", "has", "let", "mut" ]
  , Token.caseSensitive  = True
  }

literal :: Parser Literal
literal =
  (LInt <$> (try $ Token.hexadecimal lexer))
  <|> intOrFloat
  <|> (LChar <$> Token.charLiteral lexer)
  <|> (LString <$> Token.stringLiteral lexer)
  where
    intOrFloat = do
      iof <- Token.naturalOrFloat lexer
      case iof of
        Left  i -> return $ LInt i
        Right f -> return $ LFloat f

path = do
  tp <- optionMaybe typePath
  id <- name
  return $ Path (fromMaybe (TypePath []) tp) id

-- A term is the highest precendence element in an expression tree
-- Literals, named values and nested expressions fall under this category
term :: Parser UExpr
term = 
  (uliteral <$> literal)
  <|> parenUExprOrTuple
  <|> (unamed <$> path)
  <|> (ufield <$> (reservedOp "." *> name))
  <?> "expression"
  where
    -- Tries to parse an expression inside parens, if there is none parse a unit
    parenUExprOrTuple = do
      elms <- parens (sepBy expressions comma)
      case elms of
        []  -> return $ uliteral LUnit
        [e] -> return $ e
        rst -> return $ utuple rst

-- Tries to parse a chain of field accesors
field = do
  t <- term
  fs <- many (uget <$> (reservedOp "." *> name))
  case fs of
    [] -> return t
    _  -> return $ foldl (\acc x -> x acc) t fs


ifExpr =
  IfExpr <$> (reserved "if" *> expression)
         <*> (reserved "then" *> expression)
         <*> optionMaybe (reserved "else" *> expression)

pattern =
  (reservedOp "_" *> (return PWildcard))
  <|> parens pattern
  <|> constructor
  <|> bindOrAt
  <|> (PValue <$> literal)
  where
    constructor = 
      PApply <$> typePath
             <*> many pattern
    bindOrAt = do
      n <- identifier
      atpat <- optionMaybe $ reservedOp "@" *> pattern
      case atpat of
        Nothing -> return $ PBind n
        Just x  -> return $ PAt n x

caseExpr =
  CaseExpr <$> (reserved "case" *> expression <* reserved "of")
           <*> sepBy cases (reservedOp ",")
  where
    cases = (,) <$> pattern <*> (reservedOp "=>" *> expression)

letDefExpr =
  LetExpr <$> (reserved "let" *> sepBy1 def comma)
          <*> (optionMaybe $ reserved "in" *> expression)
  where
    def = (,) <$> pattern <*> (reservedOp "=" *> expression)

-- appl is the third highest precedence, this groups terms into function 
-- applications ( if any ) and it also parses flow control expressions
appl :: Parser UExpr
appl =
  (uif <$> ifExpr)
  <|> (ureturn <$> (reserved "return" *> expression))
  <|> (ucase <$> caseExpr)
  <|> (ulet <$> letDefExpr)
  <|> callify <$> many1 field
  <?> "expression"
  where
    callify [f] = f
    callify (f:as) = Free $ ECall f as

exprTable :: OperatorTable Char () UExpr
exprTable =
  [ [ po "-", pw "not" ]
  , [ lo "*", lo "/", lo "%" ]
  , [ lo "+", lo "-" ]
  , [ lo ">=", lo "<=", lo ">", lo "<" ]
  , [ lo "==", lo "!=" ]
  , [ lo "|", lo "&"]
  , [ lk "and", lk "or" ]
  , [ leftOp "<|" (\l r -> ucall l [r])
    , leftOp "|>" (\l r -> ucall r [l]) ]
  , [ eqOp ] ]
  where
    po n = prefixOp n (\val -> ucall (unamed $ toplevelName n) [val])
    pw n = prefixKw n (\val -> ucall (unamed $ toplevelName n) [val])
    lo n = leftOp n (\l r -> ucall (unamed $ toplevelName n) [l, r])
    lk n = leftKw n (\l r -> ucall (unamed $ toplevelName n) [l, r])
    eqOp = Infix (do{ reservedOp "="; return $ (\l r -> uset (setValidate l) r) }) AssocNone

-- Tries to validate the left hand side of a set operation ( assigment )
setValidate e@(Free (ENamed _)) = e
setValidate e@(Free (EGet _ _)) = e
setValidate e =
  fail $ "Invalid left hand side of assigment: " ++ show e


expression :: Parser UExpr
expression = buildExpressionParser exprTable appl

expressions :: Parser UExpr
expressions =
    usequence <$> sepEndBy1 expression (return $ uliteral LUnit) semi 
  where
    sepEndBy1 p q sep = (:) <$> p <*> many (sep *> (p <|> q))

ptype =
  
  (TFn <$> (reserved "fn" *> (sepBy atype comma))
          <*> fnret)
  <|> (reservedOp "_" *> return TUnknown)
  <|> (TParam <$> identifier)
  <|> atypeOrTuple
  <|> (TNamed <$> typePath)
  <?> "type"
  where
    fnret =
      fromMaybe <$> (return TUnit)
                <*> optionMaybe (reservedOp "->" *> atype)
    atypeOrTuple = do
      elms <- parens (sepBy atype comma)
      case elms of
        []  -> return TUnit
        [t] -> return t
        rst -> return $ TTuple rst

atype = do
  ts <- many1 ptype
  case ts of
    [t]  -> return $ t
    x:xs -> return $ TApply x xs

typePath = do
  first <- fragment
  rest  <- many ( reservedOp "::" *> fragment )
  return $ TypePath $ first : rest
  where
    fragment = nameFragment <|> applyFragment
    nameFragment = (\x -> (x, [])) <$> typeIdentifer
    applyFragment =
      parens $ (,) <$> typeIdentifer
                   <*> ((fromMaybe []) <$> (optionMaybe $ many ptype))

funcDef =
  FuncDef <$> (reserved "fn" *> (name <?> "function name"))
          <*> args
          <*> optionMaybe returnType
          <*> optionMaybe (reservedOp "=" *> expressions <?> "function body")
          <?> "function definition"
  where
    args = sepBy ( (,) <$> identifier
                       <*> (reservedOp ":" *> atype
                       <?> "argument type") ) comma
    returnType = reservedOp "->" *> atype <?> "return type"

dataDef =
  DataDef <$> (reserved "data" *> (typeIdentifer <?> "data structure name"))
          <*> (many ptype <?> "data structure type parameters")
          <*> (reservedOp "=" *> (typeCtors <?> "data structure body"))
          <?> "data structure definition"
  where
    typeCtors = sepBy1 typeCtor (reservedOp "|")
    typeCtor = (,) <$> typeIdentifer <*> many ptype

aliasDef = 
  AliasDef <$> (reserved "alias" *> (typeIdentifer <?> "type alias name"))
           <*> (many ptype <?> "type alias type parameters")
           <*> (reservedOp "=" *> atype)
           <?> "type alias"

construct = 
  (CFuncDef <$> funcDef)
  <|> (CDataDef <$> dataDef)
  <|> (CAliasDef <$> aliasDef)

program :: Parser [UConstruct]
program = whiteSpace *> many construct <* eof

parseFromString :: String -> Either ParseError [UConstruct]
parseFromString inp =
  parse program "(unknown)" inp

parseFromFile :: String -> IO (Either ParseError [UConstruct])
parseFromFile fname = 
  parse program fname <$> readFile fname

--------------------------------------------------------------------------------
--                                    Utilities                               --
--------------------------------------------------------------------------------

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser lexerStyle

parens :: Parser a -> Parser a
parens = Token.parens lexer

name = identifier <|> (try ( parens maybeReservedOp) )

identifier :: Parser String
identifier = Token.identifier lexer

typeIdentifer :: Parser String
typeIdentifer = Token.lexeme lexer ((:) <$> upper <*> many alphaNum)

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

operator = Token.operator lexer

maybeReservedOp :: Parser String
maybeReservedOp = Token.lexeme lexer (many1 $ Token.opLetter lexerStyle)

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

semi = Token.semi lexer

comma :: Parser String
comma = Token.comma lexer

integer :: Parser Integer
integer = Token.integer lexer

leftOp name fun = Infix (do{ reservedOp name; return fun }) AssocLeft
leftKw name fun = Infix (do{ reserved name; return fun }) AssocLeft
prefixOp name fun = Prefix (do{ reservedOp name; return fun })
prefixKw name fun = Prefix (do{ reserved name; return fun })
