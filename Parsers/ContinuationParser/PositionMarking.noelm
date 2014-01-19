{-
Copyright info at end of file
-}
module Parsers.ContinuationParser.PositionMarking where
{-|
This file provides the basic functionality for generating error messages with line number and column markings.

# Types
@docs PositionMarked

# Constants
@docs standardTaker, standardTakerOptions

# Functions

## PositionMarking
@docs charsToPositionMarkedChars, handlePositionMarkedInput

## Error messages
@docs parseErrorAts, parseErrorAt, markEndOfInputAsErrorAt, errorAts, errorAt

-}
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.Types
import open Parsers.ContinuationParser.Take
import open Parsers.ContinuationParser.LexemeEaters

{- types -}
type PositionMarked char =
 {line: Int
 ,column: Int
 ,char: char}

{- taker constants -}
standardTakerOptions: TakerOptions char (PositionMarked char) output
standardTakerOptions =
 {lexemeEaterTransform = handlePositionMarkedInput
 ,inputTransform = unmark}

standardTaker: Taker char (PositionMarked char) intermediate output
standardTaker = newTaker standardTakerOptions

{- position marking -}
charsToPositionMarkedChars: [Char] -> [PositionMarked Char]
charsToPositionMarkedChars chars = (\(acc,_,_) -> reverse acc) <|  foldl charToPositionMarkedChar ([],0,0) chars
charToPositionMarkedChar char (acc,line,col) =
    if | char == '\n' -> ({line=line,column=col,char=char} :: acc,line+1,0)
       | otherwise ->    ({line=line,column=col,char=char} :: acc,line,col+1)

unmark: [PositionMarked char] -> [char]
unmark = map (.char)

handlePositionMarkedInput: LexemeEater char output -> LexemeEater (PositionMarked char) output
handlePositionMarkedInput
 = anotateError (\err acc input -> errorAtM err input)
 . convertInput (\i->i.char)

{- error messages -}
parseErrorAts: Error -> [PositionMarked char] -> ParserResult input output
parseErrorAts err input = ParseError <| errorAts err input

parseErrorAt: Error -> PositionMarked char -> ParserResult input output
parseErrorAt err location = ParseError <| errorAt err location

markEndOfInputAsErrorAt: ContinuationParser (PositionMarked input) intermediate output -> Error -> ContinuationParser (PositionMarked input) intermediate output
markEndOfInputAsErrorAt continuationParser err continuation input =
 let
  parseError = parseErrorAts err input
 in
 (continuationParser `replaceEndOfInputWith` parseError) continuation input -- Why do I need those parens?

errorAts: Error -> [PositionMarked char] -> Error
errorAts err input =
 errorAtM err <| case input of
  (location::_) -> Just location
  [] -> Nothing

errorAtM: Error -> Maybe (PositionMarked char) -> Error
errorAtM err input =
 case input of
  Just location -> errorAt err location
  Nothing -> {err|message <- err.message ++ "\n    At end of input"}

errorAt: Error -> PositionMarked char -> Error
errorAt err location ={err|message<-
 err.message ++ "\n    On line:"++(show location.line)
             ++ "\n    At column:"++(show location.column)}

{-
The continuation parser
Parsec inspired continuation passing style parser

Copyright (C) 2013  Timothy Hobbs <timothyhobbs@seznam.cz> thobbs.cz

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