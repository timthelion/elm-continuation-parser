module  Parsers.ContinuationParser.Examples.ParseUserDataWithLineNumbers where

import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.Specifics.Lexemes

import String

type UserData = {name:String,location:String,occupation:String}

parseUserData: String -> ParserResult (PositionMarked Char) UserData
parseUserData unparsed
 =
 parse
  (charsToPositionMarkedChars (String.toList unparsed ++ ['\n']))
  parseUserData'

parseUserData': Parser (PositionMarked Char) UserData
parseUserData' =
 take nameField <| \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ name _ ->
 fastforward 1 <|
 take locationField <| \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ location _ ->
 fastforward 1 <|
 take occupationField <| \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ occupation _ ->
 tillEndOfInput
  (Parsed
   {name=name
   ,location=location
   ,occupation=occupation})
  <| take whitespace
  <| \ _ transition input ->
   parseErrorAts ("Unexpected input "++(show transition)++" near end of file.") input

field: String -> LexemeEater (PositionMarked Char) Char [Char]
field name = handlePositionMarkedInput <| keyword (String.toList name) (\c->c==':')
nameField = field "Name"
locationField = field "Location"
occupationField = field "Occupation"

tillEndOfLineUnpadded
 =  handlePositionMarkedInput
 <| lexeme (\c->c/='\n') (String.trim . String.fromList)