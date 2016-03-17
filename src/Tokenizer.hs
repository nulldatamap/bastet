module Tokenizer (tokenize, Token(..), TokenKind(..)
  , isStringToken, isCharToken, isIntToken, isFloatToken, isKeywordToken
  , isOperatorToken, isReservedOperatorToken, isIdentToken, isTypeIdentToken
  , reconstructSpan, reservedOperators) where

import Prelude hiding (span)

import Data.Maybe
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Numeric
import Data.Char
import Data.List hiding (span)

import Text.ParserCombinators.Parsec hiding (parseFromFile)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

import Span

data TokenKind =
  TString String
  | TChar Char
  | TInt Integer
  | TFloat Double
  | TKeyword String
  | TOperator String
  | TReservedOperator String
  | TIdent String
  | TTypeIdent String
  deriving (Show, Eq)

data Token =
  Token { tokenKind :: TokenKind
        , tokenSpan :: Span }
  deriving (Show, Eq)

isStringToken (Token (TString _) _)       = True
isStringToken _                           = False

isCharToken (Token (TChar _) _)           = True
isCharToken _                             = False

isIntToken (Token (TInt _) _)             = True
isIntToken _                              = False

isFloatToken (Token (TFloat _) _)         = True
isFloatToken _                            = False

isOperatorToken (Token (TOperator _) _)   = True
isOperatorToken _                         = False

isIdentToken (Token (TIdent _) _)         = True
isIdentToken _                            = False

isTypeIdentToken (Token (TTypeIdent _) _) = True
isTypeIdentToken _                        = False

isReservedOperatorToken x (Token (TReservedOperator o) _) = x == o
isReservedOperatorToken _ _                               = False

isKeywordToken x (Token (TKeyword k) _)                   = x == k
isKeywordToken _ _                                        = False

instance Spanable Token where
  spanOf = tokenSpan

span :: Parser (Span -> a) -> Parser a
span p = do
  st <- getPosition
  x <- p
  en <- getPosition
  return $ x (charPosToSpan st en)

hexToNum x =
  case readHex x of
    [(v, "")] -> v
    _         -> error $ "Failed to parse hex value: " ++ x

escapeSeq :: Parser Char
escapeSeq =
  char '\\' *> (char '\\'
                <|> char '\''
                <|> char '\"'
                <|> nlEsc
                <|> crEsc
                <|> tpEsc
                <|> hexEsc
                <|> unicodeEsc)
  where
    nlEsc = char 'n' *> return '\n'
    crEsc = char 'r' *> return '\r'
    tpEsc = char 't' *> return '\t'
    hexEsc =
      char 'x' *> ((chr. hexToNum) <$> ((\x y -> x:[y]) <$> hexDigit <*> hexDigit))
    unicodeEsc =
      char 'u' *> ((chr . hexToNum) <$> ((\x y z æ -> x:y:z:[æ]) <$> hexDigit 
                                                                 <*> hexDigit
                                                                 <*> hexDigit
                                                                 <*> hexDigit))


stringChar :: Char -> Parser Char
stringChar s = escapeSeq <|> noneOf ('\\' : s : whiteSpaceChars)

tkString =
  span $ Token <$> TString
      <$> between (char '"') (char '"') (many $ stringChar '\"')

tkChar =
  span $ Token <$> TChar
      <$> between (char '\'') (char '\'') (stringChar '\'')

tkNum =
  span theNum
  where
    theNum = do
      sign <- isJust <$> optionMaybe (char '-' *> return ())
      inte <- many1 digit
      flte <- optionMaybe (char '.' *> many1 digit)
      case flte of
        Just f  -> return $ Token $ TFloat $ rFloat sign $ inte ++ '.' : f
        Nothing -> return $ Token $ TInt $ rInt sign inte
    rInt s v =
      case readDec v of
            [(r, "")] -> r * si
            _         -> error $ "Failed to parse integer value: " ++ v
      where
        si = if s then -1 else 1
    rFloat s v =
      case readSigned readFloat v of
        [(r, "")] -> r * si
        _         -> error $ "Failed to parse integer value: " ++ v
      where
        si = if s then -1 else 1

identStart = lower
identLetter = (alphaNum <|> char '_')

keywords = [ "and", "or", "not", "if", "then", "else", "fn", "case", "data"
           , "alias", "return", "where", "of", "in", "instance", "trait"
           , "impl", "has", "let", "mut" ]

typeIdentStart = upper
typeIdentLetter = alphaNum

tkTypeIdent =
  span $ Token <$>
    TTypeIdent <$> ((:) <$> typeIdentStart <*> many typeIdentLetter)

tkKwOrIdent = span $ do
  nm <- ((:) <$> identStart <*> many identLetter)
  if nm `elem` keywords
  then return $ Token $ TKeyword nm
  else return $ Token $ TIdent nm

reservedOperators = [ ":", "*", "/", "%", "+", "-", "=", "&", "|", "!"
                    , "==", "<", ">", "!=", ">=", "<=", "<|", "|>", "->"
                    , "()", "_", ".", "::", ",", "@", ";", "(", ")", "["
                    , "]", "{", "}", "=>" ]

operatorLetterSingles = oneOf "[]{}()"
operatorStartLetter = (oneOf "`~!@$%^&*-+|=;:<>.,_/\\?")
operatorLetter = (oneOf "`~!@$%^&*-+|=:<>./\\?")

tkOperator = span $ do
  single <- optionMaybe operatorLetterSingles
  case single of
    Just op -> return $ Token $ TReservedOperator $ op:""
    Nothing -> combination
  where
    combination = do
      op <- (try $ string "()")
            <|> ((:) <$> operatorStartLetter <*> many operatorLetter)
      if op `elem` reservedOperators
      then return $ Token $ TReservedOperator op
      else return $ Token $ TOperator op

comment :: Parser ()
comment =
  lineComment <|> blockComment
  where
    lineComment = do
      try $ string "//"
      lineCommentBody

    lineCommentBody = do
      optional blockComment
      end <- ((== '\n') <$> anyChar)
             <|> (eof *> return True)
      if end
      then return ()
      else lineCommentBody

    blockComment = do
      try $ string "/*"
      blockCommentBody

    blockCommentBody = do
      optional blockComment
      end <- option False $ try $ string "*/" *> return True
      if end
      then return ()
      else anyChar *> blockCommentBody
      <?> "terminating '*/'"

whiteSpaceChars = " \t\n\r"
whiteSpaceChar = (oneOf whiteSpaceChars) *> return ()

ignore :: Parser ()
ignore = many (whiteSpaceChar <|> comment) *> return ()

pTokens :: Parser [Token]
pTokens =
  ignore *> (many (tk <* ignore)) <* eof
  where
    tk = tkOperator <|> tkTypeIdent
                    <|> tkKwOrIdent
                    <|> tkNum
                    <|> tkChar
                    <|> tkString

tokenize :: String -> SourceName -> Either ParseError [Token]
tokenize src nm = parse pTokens nm src

reconstructSpan :: String -> [Token] -> ParseError -> String
reconstructSpan src tokens err =
  "Syntax error: " ++ (intercalate "\n" $ map showError $ mergeErrors $ errorMessages err)
  ++ '\n' : (showSpan errTok src)
  where
    inner (SysUnExpect what) = what
    inner (UnExpect what) = what
    inner (Expect what) = what
    inner (Message what) =  what

    mergeErrors errs =
        nub $ filter ((/= "") . inner) errs

    showError (SysUnExpect what) = "got unexpected " ++ what
    showError (UnExpect what) = "got unexpected " ++ what
    showError (Expect what) = "expected " ++ what
    showError (Message errMsg) = errMsg

    errTok = head $ filter (\(Token _ s) -> enclosedBySpan s) $ tokens

    pos = errorPos err

    enclosedBySpan spn =
      ((sourceLine pos) <= (startLine spn))
      && ((sourceColumn pos) <= (startColumn spn))
