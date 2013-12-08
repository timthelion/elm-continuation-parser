{-
This module provides the fundamental functions for eating(parsing) lexemes.

Copyright information can be found at the end of this file.
-}
module Parsers.ContinuationParser.LexemeEaters where
import open Parsers.ContinuationParser
import String
import List

charset: (char -> Bool) -> LexemeEater char char [char]
charset test acc input =
 if | test input -> IncompleteLexeme
    | otherwise -> EatenLexeme {lexeme=acc,transition=input}

keyword: [Char] -> (Char->Bool) -> LexemeEater Char Char [Char]
keyword word punctuationTest acc input =
 if | punctuationTest input ->
       (if | acc == word -> EatenLexeme {lexeme=acc,transition=input}
           | otherwise ->
              LexemeError <| "Incomplete keyword: Got \""
                              ++ (String.fromList acc)
                              ++ "\" expected \""
                              ++ (String.fromList word) ++ "\"")
    | isPrefixOf (acc++[input]) word -> IncompleteLexeme
    | otherwise -> LexemeError <| "Unexpected character:"++(show input) ++ "\n Got \""
                              ++ (String.fromList (acc++[input]))
                              ++ "\" expected \""
                              ++ (String.fromList word) ++ "\""

isPrefixOf: [a] -> [a] -> Bool
isPrefixOf prefix list =
 case (prefix,list) of
  ((p::ps),(l::ls)) ->
   if | p == l -> isPrefixOf ps ls
      | otherwise -> False
  ((p::ps),[]) -> False
  ([],(l::ls)) -> True
  ([],[])      -> True

--lexeme: (char -> bool) -> ([char] -> output) -> LexemeEater char char output
lexeme test conversion acc input =
 case charset test acc input of
  EatenLexeme el -> EatenLexeme {lexeme = conversion el.lexeme, transition = el.transition}
  LexemeError err -> LexemeError err
  IncompleteLexeme -> IncompleteLexeme

--lexemeMaybe: (char -> Bool) -> ([char] -> Maybe output) -> LexemeEater char char output
lexemeMaybe test conversion acc input =
 case charset test acc input of
  EatenLexeme el ->
   case conversion acc of
    Just output -> EatenLexeme {lexeme=output,transition=input}
    Nothing -> LexemeError <| "Lexeme "++(show acc)++"failed to parse."
  LexemeError err -> LexemeError err
  IncompleteLexeme -> IncompleteLexeme

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