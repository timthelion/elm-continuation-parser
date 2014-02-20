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
import Parsers.CharacterClassification (..)
import Parsers.ContinuationParser (..)
import Parsers.ContinuationParser.Types (..)
import Parsers.ContinuationParser.Take (..)
import Parsers.ContinuationParser.LexemeEaters (..)
import Parsers.ContinuationParser.LexemeEaters as LE
import Parsers.ContinuationParser.PositionMarking (..)
import Parsers.ContinuationParser.Specifics.Lexemes (..)

t = standardTaker

{-|
Take a C style double quoted backslash escaped string.

Note: Suports only basic escape sequences.  No support for JSON style unicode escaping.
-}
takeString: ContinuationParser (PositionMarked Char) String output
takeString continuation =
 t.take (LE.expect "string" <| exactMatch ['\"']) <| \ _ ->
 (takeString' []
 `markEndOfInputAsErrorAt` {message="Matching quote not found for string.",expected = Just "close quotes"}) continuation

takeString': [Char] -> ContinuationParser (PositionMarked Char) String output
takeString' acc continuation =
 t.take normalStringSegment <| \ segment ->
  ((escapedChar <| \ escaped ->
  takeString' (acc++ segment ++ [escaped]) continuation)
               <|>
  (t.take (exactMatch ['\"']) <| \ _ ->
  continuation (String.fromList <| acc ++ segment)))

{-|

Eats a C style escaped character.  Aka "\n" becomes newline

Note: Suports only basic escape sequences.  No support for JSON style unicode escaping.

-}
escapedChar: ContinuationParser (PositionMarked Char) Char output
escapedChar continuation =
 t.take (exactMatch ['\\']) <| \ _ ->
 t.take escapedChar' continuation

{-| Eats a single line comment -}
comment: String -> ContinuationParser (PositionMarked Char) [Char] output
comment marker continuation =
 t.take (LE.expect "comment" <| exactStringMatch marker) <| \ _ ->
 t.take tillEndOfLine continuation

{-|

This eats until it reaches a quotation mark or a backslash.  AKA, it eats the easilly digestible parts of a string.

-}
normalStringSegment: LexemeEater Char [Char]
normalStringSegment = charset (\c-> c/='\"' && c/= '\\')

{-|

This eats a single character and then maps it to any associated escape sequence.  AKA 'n' becomes '/n'.

-}
escapedChar': LexemeEater Char Char
escapedChar' acc input =
 case acc of
  [] -> IncompleteLexeme <| Just "escaped character"
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
