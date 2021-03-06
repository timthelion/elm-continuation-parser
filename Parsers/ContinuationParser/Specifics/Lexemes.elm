{-
License at end of file
-}
module Parsers.ContinuationParser.Specifics.Lexemes where
{-|
This module provides useful generic LexemeEaters.

# LexemeEaters
@docs whitespace, tillEndOfLine, int, float

-}
{- Basic libraries -}
import String
import Char

{- Internal modules -}
import Parsers.CharacterClassification (..)
import Parsers.ContinuationParser (..)
import Parsers.ContinuationParser.LexemeEaters (..)
import Parsers.ContinuationParser.LexemeEaters as LE
import Parsers.ContinuationParser.Take (..)

{-| Eats any kind of whitespace. -}
whitespace: LexemeEater  Char [Char]
whitespace = LE.expect "whitespace" <| charset isWhitespace

{-| Eats until it gets to a newline -}
tillEndOfLine: LexemeEater Char [Char]
tillEndOfLine = charset (\c->c/='\n')

{-| Eats a Int style number:

Any decimal digit

-}
int: LexemeEater Char Int
int = LE.expect "int" <| lexemeMaybe (\c->Char.isDigit c) (String.toInt . String.fromList)

{-| Eats a Float style number:

Any digit or the '.' character

-}
float: LexemeEater Char Float
float = LE.expect "float" <| lexemeMaybe (\c->Char.isDigit c||c=='.') (String.toFloat . String.fromList)

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
