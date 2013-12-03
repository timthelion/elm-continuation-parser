{-

This module provides generally usefull ContinuationParsers

Copyright info at end of file

-}
module Parsers.ContinuationParser.Specifics.ContinuationParsers where

{- Base libraries -}
import String

{- Internal modules -}
import open Parsers.CharacterClassification
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.Specifics.Lexemes

--takeString: ContinuationParser (PositionMarked Char) String () output
takeString continuation = takeString' [] continuation `markEndOfInputAsErrorAt` "Matching quote not found for string."
--takeString': [Char] -> ContinuationParser (PositionMarked Char) String () output
takeString' acc continuation input =
 input |>
  (take normalStringSegment <| \ segment transition ->
   if | transition == '\\' ->
          fastforward 1
       <| take escapedChar
       <| \ escaped _ -> fastforward 1 <| takeString' (acc++ segment ++ [escaped]) continuation

      | transition == '\"' -> fastforward 1 <| continuation (String.fromList <| acc ++ segment) ())

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