module Parsers.ContinuationParser.Take where
{-|
# Lexemes

## Types
@docs LexemeEater, EatenLexeme, Taker, TakerOptions

## Functions
@newTaker

The Taker object contains the following functions:

 - take
 - takeWithFallbackValue
 - lookAhead
-}

{- internal module imports -}
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.Types


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
 ,lookAhead n continuation input = lookAheadInternal n (\intermediate -> continuation <| to.inputTransform intermediate) input
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
              EatenLexeme result ->
               if | length acc > 0 -> continue Unambiguous (continuation result) (i::is)
                  | otherwise -> continuation result (i::is) --TODO Be less naive about this...
              IncompleteLexeme -> take' takerOptions (acc++[i]) lexemeEater fallbackValue continuation is
              LexemeError err -> ParseError err
  []      -> fallbackValue

{-| Just like take, except return the given fallback value if we reach the end of input, rather than the default EndOfInputBeforeResultReached -}
takeWithFallbackValueInternal: TakerOptions preTransformInput input intermediate -> LexemeEater preTransformInput intermediate -> ParserResult input output -> ContinuationParser input intermediate output
takeWithFallbackValueInternal takerOptions lexemeEater fallbackValue continuation input = take' takerOptions [] lexemeEater fallbackValue continuation input

{-| Look ahead n characters/items/tokens in the input.   If near the end of input, returns a partial result.  Example:

If you ask for 5 chars when the remaining input is ['b','y','e'] you get only 3 chars:  ['b','y','e']-}
lookAheadInternal: Int -> ContinuationParser input [input] output
lookAheadInternal n continuation input =
 continuation (take n input) input