{-
This tutorial is intended to be a comprehensive primer in the use of the elm-continuation-parser library for readers already familiar with functional programming concepts such as:

 - recursion
 - continuation passing style
 - general data types

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
 
 - Copyright info
 - Chapter 1 - The basic types
 - Chapter 2 - Our first parser
 - Chapter 3 - Parsing LISP
 - Chapter 4 - Trampolining
 - Chapter 5 - Conclusion

# Copyright info

This tutorial was written by Timothy Hobbs in 2013.  The text portions of this tutorial are realeased under the license [Creative Commons ZERO](http://creativecommons.org/publicdomain/zero/1.0/).  The code is under the license LGPL V3 or later.
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