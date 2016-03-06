module Span (Span(..), posToSpan, spanToPos, enclosingSpan, Spanable(..)) where

import Text.ParserCombinators.Parsec.Pos
import Data.List

data Span =
  Span { spanName    :: SourceName
       , startLine   :: Line
       , endLine     :: Line
       , startColumn :: Column
       , endColumn   :: Column }
  deriving (Show, Eq)

posToSpan :: SourcePos -> SourcePos -> Span
posToSpan s e =
  Span (sourceName s)
       (sourceLine s) (sourceLine e)
       (sourceColumn s) (sourceColumn e - 1)

spanToPos :: Span -> SourcePos
spanToPos s =
  newPos (spanName s) (startLine s) (startColumn s)

enclosingSpan :: Span -> Span -> Span
enclosingSpan s e =
  Span (spanName s)
       (startLine s) (endLine e)
       (startColumn s) (endColumn e)

class Spanable x where
  spanOf :: x -> Span

  showSpan :: x -> String -> String
  showSpan x src =
    displayLines ++ '\n' : highlight
    where
      srcLines = lines src
      sp = spanOf x
      srcLineCount = 1 + (endLine sp) - (startLine sp)
      displayLines =
        intercalate "\n" $
          take (srcLineCount) $ drop (startLine sp - 1) srcLines
      highlight =
        (replicate (startColumn sp - 1) ' ') ++
          '^' : (replicate (endColumn sp - startColumn sp) '~')

