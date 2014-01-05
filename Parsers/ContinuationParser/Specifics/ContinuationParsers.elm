{-

Copyright info at end of file

-}
module Parsers.ContinuationParser.Specifics.ContinuationParsers where
{-|
This module provides generally usefull ContinuationParsers

@docs takeString, comment, escapedChar
-}

{- Base libraries -}
import String

{- Internal modules -}
import open Parsers.CharacterClassification
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.Types
import open Parsers.ContinuationParser.Take
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.Specifics.Lexemes

t = standardTaker

takeString: ContinuationParser (PositionMarked Char) String output
takeString continuation =
 t.take (exactMatch ['\"']) <| \ _ ->
 (takeString' []
 `markEndOfInputAsErrorAt` "Matching quote not found for string.") continuation

takeString': [Char] -> ContinuationParser (PositionMarked Char) String output
takeString' acc continuation =
 t.take normalStringSegment <| \ segment ->
  ((escapedChar <| \ escaped ->
  takeString' (acc++ segment ++ [escaped]) continuation)
               <|>
  (t.take (exactMatch ['\"']) <| \ _ ->
  continuation (String.fromList <| acc ++ segment)))

{-| Eats a C style escaped character.  Aka "\n" becomes newline -}
escapedChar: ContinuationParser (PositionMarked Char) Char output
escapedChar continuation =
 t.take (exactMatch ['\\']) <| \ _ ->
 t.take escapedChar' continuation

{-| Eats a single line comment -}
comment: String -> ContinuationParser (PositionMarked Char) [Char] output
comment marker continuation =
 t.take (exactStringMatch marker) <| \ _ ->
 t.take tillEndOfLine continuation

{-|

This eats untill it reaches a quotation mark or a backslash.  AKA, it eats the easilly digestible parts of a string.

-}
normalStringSegment: LexemeEater Char [Char]
normalStringSegment = charset (\c-> c/='\"' && c/= '\\')

{-|

This eats a single character and then maps it to any associated escape sequence.  AKA 'n' becomes '/n'.

-}
escapedChar': LexemeEater Char Char
escapedChar' acc input =
 case acc of
  [] -> IncompleteLexeme
  (escaped::[]) ->
   let
    output =
     if | escaped == 't' -> '\t'
        | escaped == 'n' -> '\n'
        | escaped == 'r' -> '\r'
        | escaped == '0' -> '\0'
        | otherwise -> escaped
   in
   EatenLexeme output

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