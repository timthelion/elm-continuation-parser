{- Copyright info at end of file -}
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
 t.take nameField <| \ _ ->
 t.take tillEndOfLineUnpadded <| \ name ->
 fastforward 1 <|
 t.take locationField <| \ _ ->
 t.take tillEndOfLineUnpadded <| \ location ->
 fastforward 1 <|
 t.take occupationField <| \ _ ->
 t.take tillEndOfLineUnpadded <| \ occupation ->
 tillEndOfInput
  (Parsed
   {name=name
   ,location=location
   ,occupation=occupation})
  <| t.take whitespace
  <| \ _ ->
   t.lookAhead 1 <| \ notWhitespace input ->
   parseErrorAts ("Unexpected input "++(show notWhitespace)++" near end of file.") input

field: String -> LexemeEater Char [Char]
field name = exactMatch (String.toList name)
nameField = field "Name"
locationField = field "Location"
occupationField = field "Occupation"

tillEndOfLineUnpadded
 =  lexeme (\c->c/='\n') (String.trim . String.fromList)

{-
Copyright 2013 Timothy Hobbs.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library.
-}