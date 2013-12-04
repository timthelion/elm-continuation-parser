{-
This module provides the basic datatypes and the most fundamental functions used by the continuation parser.

For copyright information see the COPYING file or the end of this file.
-}
module Parsers.ContinuationParser where

type Parser input output = [input] -> ParserResult output

data ParserResult output
 = Parsed output
 | ParseError String
 | EndOfInputBeforeResultReached

type Continuation input intermediate opinion output = intermediate -> opinion -> Parser input output

type ContinuationParser input intermediate opinion output = (Continuation input intermediate opinion output) -> Parser input output

type LexemeEater input opinion output = [input] -> input -> EatenLexeme opinion output

data EatenLexeme opinion output
 = EatenLexeme {lexeme: output, transition: opinion}
 | LexemeError String
 | IncompleteLexeme

take: LexemeEater input opinion intermediate -> ContinuationParser input intermediate opinion output
take lexemeEater continuation input = take' [] lexemeEater continuation input
take' acc lexemeEater continuation input =
 case input of
  (i::is) -> case lexemeEater acc i of
              EatenLexeme result -> continuation result.lexeme result.transition (i::is)
              IncompleteLexeme -> take' (acc++[i]) lexemeEater continuation is
              LexemeError err -> ParseError err
  [] -> EndOfInputBeforeResultReached

fastforward: Int -> Parser input output -> [input] -> ParserResult output
fastforward n parser input =
 if | n == 0 -> parser input
    | otherwise ->
       case input of
        (i::is) -> fastforward (n-1) parser is
        [] -> EndOfInputBeforeResultReached

{-| Just like the <|> in the parsec library.

First run the first parser, if it returns:
    ParseError String
 or EndOfInputBeforeResultReached

then run the seccond parser.  If that returns one of the two errors then return the first parser's error.
-}

(<|>): Parser input output -> Parser input output -> Parser input output
(<|>) parser1 parser2 input =
 case parser1 input of
  Parsed output -> Parsed output
  errorCodes ->
   case parser2 input of
    Parsed output -> Parsed output
    _ -> errorCodes

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