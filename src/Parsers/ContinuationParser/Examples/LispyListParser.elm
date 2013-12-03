{-
This module provides a sample parser created using functionality provided by the elm-continuation-parser package. This example parser can parse lispy lists. That is, trees of the form:

(this is (not a list (but a tree)))

Copyright information can be found in the COPYING file or at the end of this file.
-}

module Parsers.ContinuationParser.Examples.LispyListParser where
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.Specifics.Lexemes
import open Parsers.ContinuationParser.Specifics.ContinuationParsers
import open Parsers.CharacterClassification

import Char
import String
import List

data LispyList
 = List [LispyList]
 | Symbol String
 | Number Float
 | LispyString String

--parseLispyListFile: String ->  ParserResult [LispyList]
parseLispyListFile input = parseTopLevelLispyLists [] (charsToPositionMarkedChars <| String.toList input)

parseTopLevelLispyLists: [LispyList] -> Parser (PositionMarked Char) [LispyList]
parseTopLevelLispyLists acc input =
 let
  topLevelLists = input |>
      (take whitespace
   <| \ whitespace' transition -> -- Parsed [LispyString (String.fromList whitespace')] {-
   if | transition == ';' ->
            fastforward 1
         <| take comment
         <| \ _ _ -> parseTopLevelLispyLists acc

      | transition == '(' ->
            fastforward 1
         <| takeLispyList
         <| \ list _ -> parseTopLevelLispyLists (acc++[list])

      | otherwise -> (\input -> parseErrorAts  ("Unexpected input:" ++ (show transition)) input)
 )
 in
 case topLevelLists of
  EndOfInputBeforeResultReached -> Parsed acc
  ParseError err -> ParseError err
  Parsed output -> Parsed output

takeLispyList: ContinuationParser (PositionMarked Char) LispyList () [LispyList]
takeLispyList continuation input =
 let
  noCloseParenError = errorAts "Matching close parethesis not found for parenthesized block." input
 in
 takeLispyList' [] noCloseParenError continuation input
 
takeLispyList': [LispyList] -> String -> ContinuationParser (PositionMarked Char) LispyList () [LispyList]
takeLispyList' acc closeParenError continuation input =
 input |>
  (take whitespace <| \ _ transition ->
   if | transition == ';' ->
          fastforward 1
       <| take comment
       <| \ _ _ -> takeLispyList' acc closeParenError continuation

      | transition == '\"' ->
          fastforward 1
       <| takeString
       <| \ string _ -> takeLispyList' (acc++[LispyString string]) closeParenError continuation

      | transition == '(' ->
          fastforward 1
       <| takeLispyList
       <| \ list _ -> takeLispyList' (acc++[list]) closeParenError continuation
      | transition == ')' ->
          fastforward 1
       <| continuation (List acc) ()

      | Char.isDigit transition ->
          take float
       <| \ number' _ -> takeLispyList' (acc++[Number number']) closeParenError continuation

      | otherwise -> take
          lispySymbol
       <| \ symbol _ -> takeLispyList' (acc++[Symbol symbol]) closeParenError continuation)
  `markEndOfInputAsError` closeParenError

lispySymbol =
 symbol
  (\c->isWhitespace c || c == ')' || c == '(' || Char.isDigit c)


{-
The continuation parser
Parsec inspired continuation passing style parser

    Copyright (C) 2013  Timothy Hobbs <timothyhobbs@seznam.cz> thobbs.cz

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}