module  Parsers.ContinuationParser.Examples.ParseUserData where

import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.Specifics.Lexemes

import String

t = standardTaker

type UserData = {name:String,location:String,occupation:String}

parseUserData: String -> ParserResult (PositionMarked Char) UserData
parseUserData unparsed
 =
 parse
  (charsToPositionMarkedChars (String.toList unparsed ++ ['\n']))
  parseUserData'

parseUserData': Parser (PositionMarked Char) UserData
parseUserData' =
 t.take nameField <| \ _ _ ->
 t.take tillEndOfLineUnpadded <| \ name _ ->
 fastforward 1 <|
 t.take locationField <| \ _ _ ->
 t.take tillEndOfLineUnpadded <| \ location _ ->
 fastforward 1 <|
 t.take occupationField <| \ _ _ ->
 t.take tillEndOfLineUnpadded <| \ occupation _ ->
 tillEndOfInput
  (Parsed
   {name=name
   ,location=location
   ,occupation=occupation})
  <| t.take whitespace
  <| \ _ transition input ->
   parseErrorAts ("Unexpected input "++(show transition)++" near end of file.") input

field: String -> LexemeEater Char Char [Char]
field name = exactMatch (String.toList name)
nameField = field "Name"
locationField = field "Location"
occupationField = field "Occupation"

tillEndOfLineUnpadded
 =  lexeme (\c->c/='\n') (String.trim . String.fromList)