{-
For copyright information see the COPYING file or the end of this file.
-}
module Parsers.ContinuationParser where
{-|
This module provides the basic datatypes and the most fundamental functions used by the continuation parser.

# Parsers
## Types
@docs Parser, ParserResult, Continuation, ContinuationParser

## Functions
@docs parse, return, fastforward, lookAhead, tillEndOfInput

# Lexemes

## Types
@docs LexemeEater, EatenLexeme, Taker, TakerOptions

## Functions
@newTaker

The Taker object contains the following functions:

 - take
 - takeWithFallbackValue

# Trampolining
## Functions
@docs createSimpleContinuationThunk, createContinuationThunk, evaluateContinuations, evaluateContinuationsTill
-}
import open Lazzy

{- Fundamentals -}
type Parser input output = [input] -> ParserResult input output

{-| Values of this type exist in two forms, fully evaluated and unevaluated.  When you have a `Parser` which as you see above creates a `ParserResult` this parser result is in an unevaluated form.  You must run it through the `parser` function to get the proper evaluated form.  When a `ParserResult` is in its evaluated form, the `Continue` case is not present. -}
data ParserResult input output
 = Parsed output
 | ParseError String
 | EndOfInputBeforeResultReached
 | Continue {id:String,continuation:Lazy [input] (ParserResult input output)}

type Continuation input intermediate opinion output
 = intermediate -> opinion -> Parser input output

type ContinuationParser input intermediate opinion output
 = (Continuation input intermediate opinion output) -> Parser input output

{-| This function should be run on any `Parser` from which you would like to create a usable result.  Think of running a parser with the `parse` function line running a monad.

Note: The `ParserResult` returned will NEVER contain a `Continue`.
-}
parse: [input] -> Parser input output -> ParserResult input output
parse input parser =
 evaluateContinuations (parser input)

{-| Parse till end of input, when end of input is reached return the given ParserResult.  Good for error checks. -}
tillEndOfInput: ParserResult input output -> Parser input ignoredOutput -> Parser input output
tillEndOfInput result parser input =
 case parser input of
  EndOfInputBeforeResultReached -> result
  ParseError err -> ParseError err
  Parsed _ -> {- This shouldn't happen -} ParseError "Programmer error: End of input parsers should not return a result."

{-| Create a parser which ignores its input and returns the given result directly. -}
return: ParserResult input output -> Parser input output
return result _ = result

{-| Remove a single character/token from the input -}
fastforward: Int -> Parser input output -> [input] -> ParserResult input output
fastforward n parser input =
 if | n == 0 -> parser input
    | otherwise ->
       case input of
        (i::is) -> fastforward (n-1) parser is
        [] -> EndOfInputBeforeResultReached

{-| Look ahead n characters/items/tokens in the input.   If near the end of input, returns a partial result.  Example:

You ask for 5 chars when the remaining input is ['b','y','e'] and you get only 3 chars:  ['b','y','e']-}
lookAhead: Int -> ContinuationParser input [input] () output
lookAhead n continuation input =
 continuation (take n input) () input

{- Lexemes -}

type LexemeEater input opinion output = [input] -> input -> EatenLexeme opinion output

data EatenLexeme opinion output
 = EatenLexeme {lexeme: output, transition: opinion}
 | LexemeError String
 | IncompleteLexeme

type Taker preTransformInput input intermediate opinion output =
 {take: LexemeEater preTransformInput opinion intermediate -> ContinuationParser input intermediate opinion output
 ,takeWithFallbackValue: LexemeEater preTransformInput opinion intermediate -> ParserResult input output -> ContinuationParser input intermediate opinion output}

type TakerOptions preTransformInput input opinion output
 = {lexemeEaterTransform: LexemeEaterTransform preTransformInput input opinion output}

type LexemeEaterTransform preTransformInput postTransformInput opinion output = LexemeEater preTransformInput opinion output -> LexemeEater postTransformInput opinion output

newTaker: TakerOptions preTransformInput input opinion intermediate -> Taker preTransformInput input intermediate opinion output
newTaker to =
 let
  take = takeInternal to
  takeWithFallbackValue = takeWithFallbackValueInternal to
 in
 {take=take
 ,takeWithFallbackValue=takeWithFallbackValue}


takeInternal: TakerOptions preTransformInput input opinion intermediate -> LexemeEater preTransformInput opinion intermediate -> ContinuationParser input intermediate opinion output
takeInternal takerOptions lexemeEater continuation input = take' takerOptions [] lexemeEater EndOfInputBeforeResultReached continuation input

take': TakerOptions preTransformInput input opinion intermediate -> [input] -> LexemeEater preTransformInput opinion intermediate -> ParserResult input output -> ContinuationParser input intermediate opinion output
take' takerOptions acc lexemeEater fallbackValue continuation input =
 let
  lexemeEater' = takerOptions.lexemeEaterTransform lexemeEater
 in
 case input of
  (i::is) -> case lexemeEater' acc i of
              EatenLexeme result -> continuation result.lexeme result.transition (i::is)
              IncompleteLexeme -> take' takerOptions (acc++[i]) lexemeEater fallbackValue continuation is
              LexemeError err -> ParseError err
  []      -> fallbackValue

{-| Just like take, except return the given fallback value if we reach the end of input, rather than the default EndOfInputBeforeResultReached -}
takeWithFallbackValueInternal: TakerOptions preTransformInput input opinion intermediate -> LexemeEater preTransformInput opinion intermediate -> ParserResult input output -> ContinuationParser input intermediate opinion output
takeWithFallbackValueInternal takerOptions lexemeEater fallbackValue continuation input = take' takerOptions [] lexemeEater fallbackValue continuation input

{- Trampolining -}
{-| Create a thunk with id "" -}
createSimpleContinuationThunk: Parser input output -> [input] -> ParserResult input output
createSimpleContinuationThunk parser input = createContinuationThunk "" parser input

{-| Create a thunk with the given id -}
createContinuationThunk: String -> Parser input output -> [input] -> ParserResult input output
createContinuationThunk id parser input =
 Continue
  {id=id
  ,continuation = computeLater parser input}

{-| Evaluate all thunks returning a fully evaluated ParserResult -}
evaluateContinuations: ParserResult input output -> ParserResult input output
evaluateContinuations result =
 case result of
  Continue value ->  evaluateContinuations <| evaluate value.continuation
  _ -> result

{-| Evaluate all thunks untill a thunk with the given id is reached. Then return it, unevaluated. -}
evaluateContinuationsTill: String -> ParserResult input output -> ParserResult input output
evaluateContinuationsTill id result =
 case result of
  Continue value ->
   if | value.id == id -> Continue value
      | otherwise -> evaluateContinuationsTill id <| evaluate value.continuation
  _ -> result

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