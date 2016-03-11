module Span (Span(..), charPosToSpan, posToSpan, spanToPos, enclosingSpan
  , Spanable(..), spanEndPos) where

import Text.ParserCombinators.Parsec.Pos
import Data.List

data Span =
  Span { spanName    :: SourceName
       , startLine   :: Line
       , endLine     :: Line
       , startColumn :: Column
       , endColumn   :: Column }
  deriving (Show, Eq)

charPosToSpan s e = 
  Span (sourceName s)
       (sourceLine s) (sourceLine e)
       (sourceColumn s) (sourceColumn e - 1)

posToSpan :: SourcePos -> SourcePos -> Span
posToSpan s e =
  Span (sourceName s)
       (sourceLine s) (sourceLine e)
       (sourceColumn s) (sourceColumn e)

spanToPos :: Span -> SourcePos
spanToPos s =
  newPos (spanName s) (startLine s) (startColumn s)

spanEndPos :: Span -> SourcePos
spanEndPos s =
  newPos (spanName s) (endLine s) (endColumn s)

enclosingSpan :: Span -> Span -> Span
enclosingSpan s e =
  Span (spanName s)
       (startLine s) (endLine e)
       (startColumn s) (endColumn e)

class Show x => Spanable x where
  spanOf :: x -> Span

  showSpan :: x -> String -> String
  showSpan x src =
    body
    where
      srcLines = lines src
      sp = spanOf x
      srcLineCount = 1 + (endLine sp) - (startLine sp)
      body =
        if srcLineCount > 1
        then snd displayLines
        else (\(w, ls) -> ls ++ '\n' : highlight w) displayLines
      lineHeader =
        let headers = map (\x -> (spanName sp) ++ ":" ++ show x ++ "\t")
                        [startLine sp .. startLine sp + srcLineCount]
            widest = foldl (\acc x ->
                       if (length x) > acc
                       then (length x)
                       else acc) 0 headers
        in (widest, headers)

      displayLines =
        (w, intercalate "\n" $ map (\(x,y) -> x ++ y)
                         $ zip hs
                         $ take (srcLineCount)
                         $ drop (startLine sp - 1) srcLines)
        where
          (w, hs) = lineHeader
      highlight w =
        (replicate w ' ') ++ '\t' : 
        (replicate (sc - 1) ' ') ++ '^' : (replicate (endColumn sp - sc) '~')
        where
          sc = startColumn sp


