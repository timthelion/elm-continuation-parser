{-
This tutorial is intended to be a comprehensive primer in the use of the elm-continuation-parser library for readers already familiar with functional programming concepts such as:

 - recursion
 - continuation passing style
 - general data types

Copyright information at end of file
-}

module Parsers.ContinuationParser.Examples.LispyListTutorial where

import Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter1 as Chapter1
import Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter2 as Chapter2
import Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter3 as Chapter3

main = (\ userDataWithoutLineNumbersField
          userDataWithoutLineNumbersOutput
           ->flow down
 [Chapter1.basicTypes
 ,Chapter2.userDataWithoutLineNumbers
 ,userDataWithoutLineNumbersField
 ,userDataWithoutLineNumbersOutput
 ,Chapter3.userDataWithLineNumbers]) <~ Chapter2.userDataWithoutLineNumbersField ~ Chapter2.userDataWithoutLineNumbersOutput

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