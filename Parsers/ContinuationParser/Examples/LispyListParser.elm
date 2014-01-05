{-
This module provides a sample parser created using functionality provided by the elm-continuation-parser package. This example parser can parse lispy lists. That is, trees of the form:

(this is (not a list (but a tree)))

Copyright information can be found in the COPYING file or at the end of this file.
-}

module Parsers.ContinuationParser.Examples.LispyListParser where
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.Types
import open Parsers.ContinuationParser.Take
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.Specifics.Lexemes
import open Parsers.ContinuationParser.Specifics.ContinuationParsers
import open Parsers.CharacterClassification
import Parsers.ContinuationParser.FinalParserResult as FinalParserResult

import Char
import String
import List

data LispyList
 = List [LispyList]
 | Symbol String
 | Number Float
 | LispyString String

t = standardTaker

parseLispyListFile: String ->  FinalParserResult.FinalParserResult [LispyList]
parseLispyListFile input
 = parse
    (charsToPositionMarkedChars <| String.toList (input++"\n"))
    (parseTopLevelLispyLists [])

parseTopLevelLispyLists:
    [LispyList]
 -> Parser (PositionMarked Char) [LispyList]
parseTopLevelLispyLists acc =
      t.takeWithFallbackValue whitespace (Parsed acc)
   <| \ _ ->
    ((comment ";" `replaceEndOfInputWith` (Parsed acc) <| \ _ -> parseTopLevelLispyLists acc)
             <|>
    (takeLispyList
         <| \ list -> parseTopLevelLispyLists (acc++[list])))

takeLispyList: ContinuationParser (PositionMarked Char) LispyList [LispyList]
takeLispyList continuation =
 t.take (exactMatch ['(']) <| \ _ ->
 ((takeLispyList' []) `markEndOfInputAsErrorAt` "Matching close parethesis not found for parenthesized block.") continuation
 
takeLispyList': [LispyList] -> ContinuationParser (PositionMarked Char) LispyList [LispyList]
takeLispyList' acc continuation =
  t.take whitespace <| \ _ ->
   ((comment ";"
       <| \ _ -> takeLispyList' acc continuation)
   <|> (takeString
       <| \ string -> takeLispyList' (acc++[LispyString string]) continuation)
   <|> (takeLispyList
       <| \ list -> takeLispyList' (acc++[list]) continuation)
   <|> (t.take (exactMatch [')'])
       <| \ _ -> continuation (List acc))
   <|> (t.take float
       <| \ number' -> takeLispyList' (acc++[Number number']) continuation)
   <|> (t.take lispySymbol
       <| \ symbol -> takeLispyList' (acc++[Symbol symbol]) continuation))

lispySymbol =
 symbol
  (\c->isWhitespace c || c == ')' || c == '(' || Char.isDigit c)


{-
The continuation parser
Parsec inspired continuation passing style parser

Copyright (C) 2013  Timothy Hobbs <timothyhobbs@seznam.cz> thobbs.cz

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library.
-}