{-
This file provides the basic functionality for ataching line numbers and collumn numbers to error messages generated by the parser.

Copyright info at end of file
-}
module Parsers.ContinuationParser.PositionMarking where
import open Parsers.ContinuationParser
import open Lazy

type PositionMarked char =
 {line: Int
 ,column: Int
 ,char: char}

charsToPositionMarkedChars: [Char] -> [PositionMarked Char]
charsToPositionMarkedChars chars = (\(acc,_,_) -> reverse acc) <|  foldl charToPositionMarkedChar ([],0,0) chars
charToPositionMarkedChar char (acc,line,col) =
    if | char == '\n' -> ({line=line,column=col,char=char} :: acc,line+1,0)
       | otherwise ->    ({line=line,column=col,char=char} :: acc,line,col+1)

handlePositionMarkedInput: LexemeEater char char output -> LexemeEater (PositionMarked char) char output
handlePositionMarkedInput unmarkedLexemeEater =
 (\acc input ->
   let
    unmarkedAcc = map (.char) acc
    unmarkedInput = input.char
   in
   case unmarkedLexemeEater unmarkedAcc unmarkedInput of
    LexemeError err -> LexemeError <| errorAt err input
    EatenLexeme lexeme -> EatenLexeme lexeme
    IncompleteLexeme -> IncompleteLexeme)

parseErrorAt: String -> PositionMarked char -> ParserResult input output
parseErrorAt message location = ParseError <| errorAt message location

parseErrorAts: String -> [PositionMarked char] -> ParserResult input output
parseErrorAts message input = ParseError <| errorAts message input

errorAt: String -> PositionMarked char -> String
errorAt message location =
 message ++ "\n    On line:"++(show location.line)
         ++ "\n    At column:"++(show location.column)

errorAts: String -> [PositionMarked char] -> String
errorAts message input =
 case input of
  (location::_) -> errorAt message location
  [] -> message ++ "\n    At end of input"

--markEndOfInputAsErrorAt: ContinuationParser input intermediate opinion output -> String -> (Continuation input intermediate opinion output) -> Parser input output
markEndOfInputAsErrorAt continuationparser message continuation' input =
 let
  continuationId = errorAts message input
  --continuation: Continuation input intermediate opinion output
  continuation intermediate opinion input' =
      Continue
       {id = continuationId
       ,continuation = computeLater
         (continuation' intermediate opinion)
         input'}
 in
 case  evaluateContinuationsTill continuationId <| continuationparser continuation input of
  EndOfInputBeforeResultReached ->
   parseErrorAts message input
  otherCases -> otherCases

--markEndOfInputAsError: Parser input output -> String -> Parser input output
markEndOfInputAsError parser message input =
 case parser input of
  EndOfInputBeforeResultReached -> ParseError message
  otherCases -> otherCases

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