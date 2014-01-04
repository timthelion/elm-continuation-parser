{-
License at end of file
-}
module Parsers.ContinuationParser.Specifics.Lexemes where
{-|
This module provides useful generic LexemeEaters.

# LexemeEaters
@docs whitespace, tillEndOfLine, int, float, symbol

-}
{- Basic libraries -}
import String
import Char

{- Internal modules -}
import open Parsers.CharacterClassification
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters

{-| Eats any kind of whitespace. -}
whitespace: LexemeEater  Char [Char]
whitespace = charset isWhitespace

{-| Eats untill it gets to a newline -}
tillEndOfLine: LexemeEater Char [Char]
tillEndOfLine = charset (\c->c/='\n')

{-| Eats a Int style number:

Any decimal digit

-}
int: LexemeEater Char Int
int = lexemeMaybe (\c->Char.isDigit c) (String.toInt . String.fromList)

{-| Eats a Float style number:

Any digit or the '.' character

-}
float: LexemeEater Char Float
float = lexemeMaybe (\c->Char.isDigit c||c=='.') (String.toFloat . String.fromList)

{-|

This eats untill it reaches punctuation of your choice.  It then converts what it's eaten to a String.

 -}
symbol: (Char -> Bool) -> LexemeEater Char String
symbol punctuationTest = symbol' punctuationTest
symbol' punctuationTest = lexeme (not . punctuationTest) String.fromList

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