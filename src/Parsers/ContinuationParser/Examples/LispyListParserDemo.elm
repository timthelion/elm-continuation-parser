module Parsers.ContinuationParser.Examples.LispyListParserDemo where
import open Parsers.ContinuationParser.Examples.LispyListParser
import Graphics.Input

(f,fs) = Graphics.Input.fieldMultiline "Enter lispy list to parse here."
main = (\f fs -> flow down [f, asText <| parseLispyListFile fs]) <~ f ~ fs