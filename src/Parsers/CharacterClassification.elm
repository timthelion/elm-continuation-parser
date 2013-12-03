module Parsers.CharacterClassification where
import Char

isWhitespace: Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isLetter: Char -> Bool
isLetter c = Char.isUpper c || Char.isLower c