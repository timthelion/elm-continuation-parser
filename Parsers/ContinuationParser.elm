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
@docs parse, return, fastforward, lookAhead, tillEndOfInput, <|>

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
{- base imports -}
import Trampoline
import Either

{- internal modules -}
import open Lazzy

{- Fundamentals -}
type Parser input output = [input] -> ParserResult input output

{-| Values of this type exist in two forms, fully evaluated and unevaluated.  When you have a `Parser` which as you see above creates a `ParserResult` this parser result is in an unevaluated form.  You must run it through the `parser` function to get the proper evaluated form.  When a `ParserResult` is in its evaluated form, the `Continue` case is not present. -}
data ParserResult input output
 = Parsed output
 | ParseError String
 | EndOfInputBeforeResultReached
 | Continue {id:String,continuation:Lazy [input] (ParserResult input output)}

type Continuation input intermediate output
 = intermediate -> Parser input output

type ContinuationParser input intermediate output
 = (Continuation input intermediate output) -> Parser input output

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

If you ask for 5 chars when the remaining input is ['b','y','e'] you get only 3 chars:  ['b','y','e']-}
lookAhead': Int -> ContinuationParser input [input] output
lookAhead' n continuation input =
 continuation (take n input) input

{- Lexemes -}

type LexemeEater input output = [input] -> input -> EatenLexeme output

data EatenLexeme output
 = EatenLexeme output
 | LexemeError String
 | IncompleteLexeme

type Taker preTransformInput input intermediate output =
 {take: LexemeEater preTransformInput intermediate -> ContinuationParser input intermediate output
 ,takeWithFallbackValue: LexemeEater preTransformInput intermediate -> ParserResult input output -> ContinuationParser input intermediate output
 ,lookAhead: Int -> ContinuationParser input [preTransformInput] output}

type TakerOptions preTransformInput input output
 = {lexemeEaterTransform: LexemeEaterTransform preTransformInput input output
   ,inputTransform: [input] -> [preTransformInput]}

type LexemeEaterTransform preTransformInput postTransformInput output = LexemeEater preTransformInput output -> LexemeEater postTransformInput output

newTaker: TakerOptions preTransformInput input intermediate -> Taker preTransformInput input intermediate output
newTaker to =
 let
  take = takeInternal to
  takeWithFallbackValue = takeWithFallbackValueInternal to
 in
 {take=take
 ,takeWithFallbackValue=takeWithFallbackValue
 ,lookAhead n continuation input = lookAhead' n (\intermediate -> continuation <| to.inputTransform intermediate) input
 }


takeInternal: TakerOptions preTransformInput input intermediate -> LexemeEater preTransformInput intermediate -> ContinuationParser input intermediate output
takeInternal takerOptions lexemeEater continuation input = take' takerOptions [] lexemeEater EndOfInputBeforeResultReached continuation input

take': TakerOptions preTransformInput input intermediate -> [input] -> LexemeEater preTransformInput intermediate -> ParserResult input output -> ContinuationParser input intermediate output
take' takerOptions acc lexemeEater fallbackValue continuation input =
 let
  lexemeEater' = takerOptions.lexemeEaterTransform lexemeEater
 in
 case input of
  (i::is) -> case lexemeEater' acc i of
              EatenLexeme result -> createSimpleContinuationThunk (continuation result) (i::is)
              IncompleteLexeme -> take' takerOptions (acc++[i]) lexemeEater fallbackValue continuation is
              LexemeError err -> ParseError err
  []      -> fallbackValue

{-| Just like take, except return the given fallback value if we reach the end of input, rather than the default EndOfInputBeforeResultReached -}
takeWithFallbackValueInternal: TakerOptions preTransformInput input intermediate -> LexemeEater preTransformInput intermediate -> ParserResult input output -> ContinuationParser input intermediate output
takeWithFallbackValueInternal takerOptions lexemeEater fallbackValue continuation input = take' takerOptions [] lexemeEater fallbackValue continuation input

infixl 0 <|>
{-|
This is the standard parser choice operator.  It alows you to say:
 Try the parser on the left,
 and if that fails try the one on the right.

It is a little bit more flexible than parsec's <|>.
Whereas in parsec <|> is LL(1) by default in the elm continuation parser it is LL(k).  The elm continuation parser is satisfied that the choice is not ambiguous when a parser returns Continue.

Notes:
 - take returns Continue at the end of each lexeme by default, so the choice operator is satisfied after a lexeme is eaten.
 - You can do something similar to `try` by using evaluateContinuationsTill to evaluate Continues untill a given Continue id, thus preventing <|> from believing that the choice is unambiguous.
  + Warning: evaluateContinuationsTill does not stack well.  For example, you must be very carefull if you use PositionMarking's markEndOfInputAsErrorAt in conjunction with it. 
-}
(<|>) : Parser input output-> Parser input output -> Parser input output
(<|>) p1 p2 input =
 transformError (p1 input) <| \ err ->
 transformError (p2 input) <| \ _ -> err

{-| If the ParserResult is EndOfInputBeforeResultReached or ParseError apply the given function to that error. -}
transformError: ParserResult input output -> (ParserResult input output -> ParserResult input output) -> ParserResult input output
transformError result transformation =
 case result of
  EndOfInputBeforeResultReached -> transformation result
  ParseError err -> transformation result
  _ -> result

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
 Trampoline.trampoline result <| \ result ->
 case result of
  Continue value ->  Either.Left <| evaluate value.continuation
  _ -> Either.Right result

{-| Evaluate all thunks untill a thunk with the given id is reached. Then return it, unevaluated. -}
evaluateContinuationsTill: String -> ParserResult input output -> ParserResult input output
evaluateContinuationsTill id result =
 Trampoline.trampoline result <| \ result ->
 case result of
  Continue value ->
   if | value.id == id -> Either.Right <| Continue value
      | otherwise -> Either.Left <| evaluate value.continuation
  _ -> Either.Right result

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