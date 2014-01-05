{-
Copyright information can be found at the end of this file.
-}
module Parsers.ContinuationParser.LexemeEaters where
{-|
This module provides the fundamental functions for eating(parsing) lexemes.

@docs charset, keyword, lexeme, lexemeMaybe, convertOutput, convertOutputMaybe, untill, untillMarker, exactMatch
-}

import open Parsers.ContinuationParser.Take
import String
import List

{-| Eat anything that passes the test. -}
charset: (char -> Bool) -> LexemeEater char [char]
charset test acc input =
 if | test input -> IncompleteLexeme
    | otherwise -> EatenLexeme acc

{-| Eat anything that matches the list.  This function takes two parameters:

 - The keyword to be eaten
 - The punctuation test

All keywords must end in punctuation.  Otherwise we would end up in the situation that "assumption" would match the keyword "as". -}
keyword: [Char] -> (Char->Bool) -> LexemeEater Char [Char]
keyword word punctuationTest acc input =
 if | punctuationTest input ->
       (if | acc == word -> EatenLexeme acc
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

isSuffixOf: [a] -> [a] -> Bool
isSuffixOf suffix list =
 isPrefixOf (reverse suffix) (reverse list)

{-| Eat anything that passes the test, then use a conversion function to turn it into a more usefull intermediate value -}
lexeme: (char -> Bool) -> ([char] -> output) -> LexemeEater char output
lexeme test conversion =
 convertOutput conversion (charset test)

{-| Eat anything that passes the test, then use a conversion function to turn it into a more usefull intermediate value.  If the conversion function returns Nothing, throw a parse error.-}
lexemeMaybe: (char -> Bool) -> ([char] -> Maybe output) -> LexemeEater char output
lexemeMaybe test conversion =
 convertOutputMaybe conversion (charset test)

{-| Convert the output of a LexemeEater using the given conversion funciton -}
convertOutput: (output -> convertedOutput) -> LexemeEater char output -> LexemeEater char convertedOutput
convertOutput conversion eater acc input =
 case (eater acc input) of
  EatenLexeme el -> EatenLexeme <| conversion el
  LexemeError err -> LexemeError err
  IncompleteLexeme -> IncompleteLexeme

{-| Try to convert the output of the given lexeme eater.  If the conversion conversion fails(returns Nothing), report an error. -}
convertOutputMaybe: (output -> Maybe convertedOutput) -> LexemeEater char output -> LexemeEater char convertedOutput
convertOutputMaybe conversion eater acc input =
 case eater acc input of
  EatenLexeme el ->
   case conversion el of
    Just output -> EatenLexeme output
    Nothing -> LexemeError <| "Lexeme "++(show acc)++"failed to parse."
  LexemeError err -> LexemeError err
  IncompleteLexeme -> IncompleteLexeme

{-| Eat untill the condition is met.  The condition takes the currently consumed input and returns a Bool. -}
untill: ([char]->Bool) -> LexemeEater char [char]
untill test acc input =
 if | test acc  -> EatenLexeme acc
    | otherwise -> IncompleteLexeme

{-| Eat untill the given marker(list segment) is reached, then return the eaten contents except for the segment. -}
untillMarker: [char] -> LexemeEater char [char]
untillMarker marker acc input =
 case untill (\acc -> isSuffixOf marker acc) acc input of
  EatenLexeme le -> EatenLexeme <| take (length le - length marker) le
  IncompleteLexeme -> IncompleteLexeme
 
exactMatch: [char] -> LexemeEater char [char]
exactMatch patern acc input =
 if | acc == patern -> EatenLexeme patern
    | isPrefixOf (acc++[input]) patern -> IncompleteLexeme
    | otherwise -> LexemeError <| "Unexpected input:" ++ show input

exactStringMatch: String -> LexemeEater Char [Char]
exactStringMatch s = exactMatch (String.toList s)

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