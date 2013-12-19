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
import Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter4 as Chapter4
import Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter5 as Chapter5

tableOfContents = [markdown|
# The continuation parser ([download/source](https://github.com/timthelion/elm-continuation-parser))

**Table of Contents**

 - Chapter 1 - The basic types
 - Chapter 2 - Our first parser
 - Chapter 3 - Parsing LISP
 - Chapter 4 - Trampolining
 - Chapter 5 - Conclusion
|]

main = (\ userDataField
          userDataOutput
          lispyListParserWithoutTrampoliningField
          lispyListParserWithoutTrampoliningOutput
          lispyListParserField
          lispyListParserOutput
           ->flow down
 [tableOfContents
 ,Chapter1.basicTypes
 ,Chapter2.userData
 ,userDataField
 ,userDataOutput
 ,Chapter3.parsingLispyLists
 ,lispyListParserWithoutTrampoliningField
 ,lispyListParserWithoutTrampoliningOutput
 ,Chapter4.trampolining
 ,lispyListParserField
 ,lispyListParserOutput
 ,Chapter5.conclusion])
 <~ Chapter2.userDataField
 ~  Chapter2.userDataOutput
 ~  Chapter3.lispyListParserWithoutTrampoliningField
 ~  Chapter3.lispyListParserWithoutTrampoliningOutput
 ~  Chapter4.lispyListParserField
 ~  Chapter4.lispyListParserOutput

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