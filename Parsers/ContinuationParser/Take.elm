module Parsers.ContinuationParser.Take where
{-|
# Lexemes

## Types
@docs LexemeEater, EatenLexeme, Taker, TakerOptions

## Functions
@newTaker

The Taker object contains the following functions:

 - take
 - disambiguate
 - lookAhead
-}

{- internal module imports -}
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.Types


{- Lexemes -}

type LexemeEater input output = [input] -> Maybe input -> EatenLexeme output

data EatenLexeme output
 = EatenLexeme output
 | LexemeError Error
 | IncompleteLexeme Expectation

type Taker preTransformInput input intermediate output =
 {take: LexemeEater preTransformInput intermediate -> ContinuationParser input intermediate output
 ,disambiguate: (preTransformInput -> Bool) -> Parser input output -> Parser input output
 ,lookAhead: Int -> ContinuationParser input [preTransformInput] output}

type TakerOptions preTransformInput input output
 = {lexemeEaterTransform: LexemeEaterTransform preTransformInput input output
   ,inputTransform: [input] -> [preTransformInput]}

type LexemeEaterTransform preTransformInput postTransformInput output = LexemeEater preTransformInput output -> LexemeEater postTransformInput output

newTaker: TakerOptions preTransformInput input intermediate -> Taker preTransformInput input intermediate output
newTaker to =
 let
  lookAhead n continuation input =
   lookAheadInternal n (\intermediate -> continuation <| to.inputTransform intermediate) input
 in
 {take = takeInternal to
 ,disambiguate test parser input =
    input |> (lookAhead 1 <| \ future ->
    case future of
     [] -> \ _ -> EndOfInputBeforeResultReached Nothing
     (i::[]) ->
      if | test i -> continue Unambiguous <| \ _ -> parser input
         | otherwise -> \ _ -> ParseError {message="",expected=Nothing})
 ,lookAhead = lookAhead
 }

takeInternal: TakerOptions preTransformInput input intermediate -> LexemeEater preTransformInput intermediate -> ContinuationParser input intermediate output
takeInternal takerOptions lexemeEater continuation input = take' takerOptions [] lexemeEater continuation input

take': TakerOptions preTransformInput input intermediate -> [input] -> LexemeEater preTransformInput intermediate -> ContinuationParser input intermediate output
take' takerOptions acc lexemeEater continuation input =
 let
  lexemeEater' = takerOptions.lexemeEaterTransform lexemeEater
  determineAmbiguity result =
   if | length acc > 0 -> continue Unambiguous (continuation result) input
      | otherwise -> continuation result input --TODO Be less naive about this...
 in
 case input of
  (i::is) -> case lexemeEater' acc (Just i) of
              EatenLexeme result -> determineAmbiguity result
              IncompleteLexeme expectation -> take' takerOptions (acc++[i]) lexemeEater continuation is
              LexemeError err -> ParseError err
  []      ->
   case lexemeEater' acc Nothing of
    EatenLexeme result -> determineAmbiguity result
    IncompleteLexeme expectation -> EndOfInputBeforeResultReached expectation
    LexemeError err -> ParseError err

{-| Look ahead n characters/items/tokens in the input.   If near the end of input, returns a partial result.  Example:

If you ask for 5 chars when the remaining input is ['b','y','e'] you get only 3 chars:  ['b','y','e']-}
lookAheadInternal: Int -> ContinuationParser input [input] output
lookAheadInternal n continuation input =
 continuation (take n input) input