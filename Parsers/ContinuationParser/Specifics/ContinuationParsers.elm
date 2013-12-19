{-

Copyright info at end of file

-}
module Parsers.ContinuationParser.Specifics.ContinuationParsers where
{-|
This module provides generally usefull ContinuationParsers

@docs takeString
-}

{- Base libraries -}
import String

{- Internal modules -}
import open Parsers.CharacterClassification
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.Specifics.Lexemes

t = standardTaker

takeString: ContinuationParser (PositionMarked Char) String Char output
takeString = 
 takeString' []
 `markEndOfInputAsErrorAt` "Matching quote not found for string."

takeString': [Char] -> ContinuationParser (PositionMarked Char) String Char output
takeString' acc continuation input =
 input |>
  (t.take normalStringSegment <| \ segment transition ->
   if | transition == '\\' ->
          fastforward 1
       <| t.take escapedChar
       <| \ escaped _ -> takeString' (acc++ segment ++ [escaped]) continuation

      | transition == '\"' ->
            fastforward 1
         <| continuation (String.fromList <| acc ++ segment) '\"')

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