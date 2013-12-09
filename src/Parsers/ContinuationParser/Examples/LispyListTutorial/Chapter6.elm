module Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter6 where

conclusion = [markdown|
# Conclusion - Larger projects using this parsing library

I would like to bring your attention to the reason I wrote the elm-continuation-parser in the first place.  I am working on a visual programming language called [graphical-elm](http://www.thobbs.cz/works/2013/graphical-elm/Intro.html).  It produces Elm files which function both as compiled output and as saved data.  While I sufficed with just split, fold and map I felt that the parsing code would look nicer, be more maintainable and produce nicer output if it was written with some sort of parsing library.  None existing in Elm yet, I wrote my own.  You can see the source code to graphical-elm's parser [here](https://github.com/timthelion/graphical-elm/tree/master/src/ParserAndCompiler).
|]