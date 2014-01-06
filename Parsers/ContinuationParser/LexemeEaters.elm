{-
Copyright information can be found at the end of this file.
-}
module Parsers.ContinuationParser.LexemeEaters where
{-|
This module provides generic functions for building and modifying LexemEaters.

#Types of lexemes
@docs charset, keyword, lexeme, lexemeMaybe, symbol, untill, untillMarker, exactMatch, endOfInput

#Modifying LexemeEaters
@docs convertOutput, convertOutputMaybe, convertInput, anotateError, whenNothingWasEaten, catchEndOfInput
-}

import open Parsers.ContinuationParser.Take
import String
import open List.CPSExtras

{- types of lexeme -}
{-| Eat anything that passes the test.
If it reaches the end of the input before the test fails, take will return EndofinputBeforeResultReached.
You can change this behavior with catchEndOfInput.
-}
charset: (char -> Bool) -> LexemeEater char [char]
charset test acc input' =
 case input' of
  Just input ->
   if | test input -> IncompleteLexeme
      | otherwise -> EatenLexeme acc
  Nothing -> IncompleteLexeme

{-| Eat anything that matches the list.  This function takes two parameters:

 - The keyword to be eaten
 - The punctuation test

All keywords must end in punctuation or end of input.  Otherwise we would end up in the situation that "assumption" would match the keyword "as". -}
keyword: [Char] -> (Char->Bool) -> LexemeEater Char [Char]
keyword word punctuationTest acc input' =
 let
  punctuationTestSuccess =
   if | acc == word -> EatenLexeme acc
      | otherwise ->
         LexemeError <| "Incomplete keyword: Got \""
                     ++ (String.fromList acc)
                     ++ "\" expected \""
                     ++ (String.fromList word) ++ "\""
 in
 case input' of
  Just input ->
   if | punctuationTest input -> punctuationTestSuccess
      | isPrefixOf (acc++[input]) word -> IncompleteLexeme
      | otherwise -> LexemeError <| "Unexpected character:"++(show input) ++ "\n Got \""
                                ++ (String.fromList (acc++[input]))
                                ++ "\" expected \""
                                ++ (String.fromList word) ++ "\""
  Nothing -> punctuationTestSuccess

{-| Eat anything that passes the test, then use a conversion function to turn it into a more usefull intermediate value.

- Returns an error if nothing is eaten
-}
lexeme: (char -> Bool) -> ([char] -> output) -> LexemeEater char output
lexeme test conversion
 = whenNothingWasEaten (LexemeError <| "Error invalid lexeme") -- TODO improve this error
 <| convertOutput conversion (charset test)

{-| Eat anything that passes the test, then use a conversion function to turn it into a more usefull intermediate value.

- Returns an error if nothing is eaten
- Returns an error if the conversion function returns Nothing.

-}
lexemeMaybe: (char -> Bool) -> ([char] -> Maybe output) -> LexemeEater char output
lexemeMaybe test conversion
 = whenNothingWasEaten (LexemeError <| "Error invalid lexeme") -- TODO improve this error
 <| convertOutputMaybe conversion (charset test)

{-|

This eats untill it reaches punctuation of your choice.  It then converts what it's eaten to a String.

Based on the lexeme function.
 -}
symbol: (Char -> Bool) -> LexemeEater Char String
symbol punctuationTest = symbol' punctuationTest
symbol' punctuationTest = lexeme (not . punctuationTest) String.fromList

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
 
{-|
Eats an exact patern.

Note: if you want to parse a keyword, see the keyword function instead.
-}
exactMatch: [char] -> LexemeEater char [char]
exactMatch patern acc input' =
 case input' of
  Just input ->
   if | isPrefixOf (acc++[input]) patern -> IncompleteLexeme
      | acc == patern -> EatenLexeme patern
      | otherwise -> LexemeError <| "Unexpected input:" ++ show input ++ " expected:" ++ show patern
  Nothing ->
   if | acc == patern -> EatenLexeme patern
      | otherwise -> LexemeError <| "Unexpected end of input. Expected:" ++ show patern

{-|
Eats an exact patern.

Note: if you want to parse a keyword, see the keyword function instead.
-}
exactStringMatch: String -> LexemeEater Char [Char]
exactStringMatch s = exactMatch (String.toList s)

{-| Succeeds if at end of input.  Otherwise throws unexpected input error. -}
endOfInput: LexemeEater input ()
endOfInput acc input =
 case input of
  Nothing -> EatenLexeme ()
  Just i -> LexemeError <| "Expected end of input, instead received:" ++ show i

{- modifying LexemeEaters -}
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

convertInput: (input -> convertedInput) -> LexemeEater convertedInput output -> LexemeEater input output
convertInput convert lexemeEater acc input' =
   let
    convertedAcc = map convert acc
    convertedInput =
     case input' of
      Just input -> Just <| convert input
      Nothing -> Nothing
   in
   lexemeEater convertedAcc convertedInput

anotateError: (String -> [char] -> Maybe char -> String) -> LexemeEater char output -> LexemeEater char output
anotateError anotate lexemeEater acc input =
   case lexemeEater acc input of
    LexemeError err -> LexemeError <| anotate err acc input
    EatenLexeme lexeme -> EatenLexeme lexeme
    IncompleteLexeme -> IncompleteLexeme

whenNothingWasEaten: EatenLexeme output -> LexemeEater input output -> LexemeEater input output
whenNothingWasEaten nothingEaten lexemeEater acc input =
 case lexemeEater acc input of
  IncompleteLexeme -> IncompleteLexeme
  LexemeError err -> LexemeError err
  EatenLexeme lexeme ->
   if | length acc > 0 -> EatenLexeme lexeme
      | otherwise -> nothingEaten

catchEndOfInput: EatenLexeme output -> LexemeEater input output -> LexemeEater input output
catchEndOfInput reaction lexemEater acc input =
 case input of
  Just _ -> lexemEater acc input
  Nothing -> reaction
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