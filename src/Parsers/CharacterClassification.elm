{-
This module provides methods for identifying the types of Chars beyond the basic functions provided in the Char module.

Copyright AGPL v 3.0 Timothy Hobbs see end of file or the COPYING file for copyright information. -}
module Parsers.CharacterClassification where
import Char

isWhitespace: Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isLetter: Char -> Bool
isLetter c = Char.isUpper c || Char.isLower c

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
