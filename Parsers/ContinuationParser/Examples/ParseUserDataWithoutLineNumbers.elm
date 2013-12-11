module Parsers.ContinuationParser.Examples.ParseUserDataWithoutLineNumbers where

import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.CharacterClassification

import String

whitespace = charset isWhitespace

{-
whitespace: LexemeEater Char Char [Char]
whitespace consumed input =
 if | isWhitespace input -> IncompleteLexeme
    | otherwise -> EatenLexeme {lexeme=consumed,transition=input}
--}

type UserData = {name:String,location:String,occupation:String}

parseUserData: String -> ParserResult Char UserData
parseUserData unparsed =
 parse (String.toList unparsed ++ ['\n']) parseUserData'

parseUserData': Parser Char UserData
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
   {name = name
   ,location = location
   ,occupation = occupation
   })
  <| take whitespace
  <| \ _ transition _ ->
    ParseError ("Unexpected input "++(show transition)++" near end of file.")

field: String -> LexemeEater Char Char [Char]
field name = keyword (String.toList name) (\c->c==':')
nameField = field "Name"
locationField = field "Location"
occupationField = field "Occupation"

tillEndOfLineUnpadded = lexeme (\c->c/='\n') (String.trim . String.fromList)

