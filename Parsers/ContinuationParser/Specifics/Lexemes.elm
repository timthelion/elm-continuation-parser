{-
This module provides useful generic LexemeEaters.

License at end of file
-}
module Parsers.ContinuationParser.Specifics.Lexemes where

{- Basic libraries -}
import String
import Char

{- Internal modules -}
import open Parsers.CharacterClassification
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking


{-| This is not a LexemeEater, it is just a nice utility function. -}
--positionMarkedCharset: (char -> Bool) -> LexemeEater (PositionMarked char) char [char]
positionMarkedCharset test = handlePositionMarkedInput <| charset test

{-| Eats any kind of whitespace. -}
--whitespace: LexemeEater (PositionMarked Char) Char [Char]
whitespace = positionMarkedCharset isWhitespace

{-| Eats untill it gets to a newline, good for one line comments -}
--comment: LexemeEater (PositionMarked Char) Char [Char]
comment = positionMarkedCharset (\c->c/='\n')

{-| Eats a Float style number:

Any digit or the '.' character

-}
--float: LexemeEater (PositionMarked Char) Char Float
float = handlePositionMarkedInput <| lexemeMaybe (\c->Char.isDigit c||c=='.') (String.toFloat . String.fromList)

{-|

This eats untill it reaches punctuation of your choice.  It then converts what it's eaten to a String.

 -}
--symbol: (Char -> Bool) -> LexemeEater (PositionMarked Char) Char String
symbol punctuationTest = handlePositionMarkedInput (symbol' punctuationTest)
symbol' punctuationTest = lexeme (not . punctuationTest) String.fromList

{-|

This eats untill it reaches a quotation mark or a backslash.  AKA, it eats the easilly digestible parts of a string.

-}
normalStringSegment: LexemeEater (PositionMarked Char) Char [Char]
normalStringSegment = positionMarkedCharset (\c-> c/='\"' && c/= '\\')

{-|

This eats a single character and then maps it to any associated escape sequence.  AKA 'n' becomes '/n'.

-}
--escapedChar: LexemeEater (PositionMarked Char) Char Char
escapedChar acc input =
 case acc of
  [] -> IncompleteLexeme
  (escaped::[]) ->
   let
    output =
     if | escaped.char == 't' -> '\t'
        | escaped.char == 'n' -> '\n'
        | escaped.char == 'r' -> '\r'
        | escaped.char == '0' -> '\0'      
        | otherwise -> escaped.char
   in
   EatenLexeme {lexeme=output,transition=input}

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