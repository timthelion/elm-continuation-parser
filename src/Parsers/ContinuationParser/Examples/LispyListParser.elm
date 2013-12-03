module Parsers.ContinuationParser.Examples.LispyListParser where
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.VariousParsersAndLexemeEaters
import open Parsers.CharacterClassification

import Char
import String
import List

data LispyList
 = List [LispyList]
 | Symbol String
 | Number Float
 | LispyString String

--parseLispyListFile: String ->  ParserResult [LispyList]
parseLispyListFile input = parseTopLevelLispyLists [] (charsToPositionMarkedChars <| String.toList input)

parseTopLevelLispyLists: [LispyList] -> Parser (PositionMarked Char) [LispyList]
parseTopLevelLispyLists acc input =
 let
  topLevelLists = input |>
      (take whitespace
   <| \ whitespace' transition -> -- Parsed [LispyString (String.fromList whitespace')] {-
   if | transition == ';' ->
            fastforward 1
         <| take comment
         <| \ _ _ -> parseTopLevelLispyLists acc

      | transition == '(' ->
            fastforward 1
         <| takeLispyList
         <| \ list _ -> parseTopLevelLispyLists (acc++[list])

      | otherwise -> (\input -> parseErrorAts  ("Unexpected input:" ++ (show transition)) input)
 )
 in
 case topLevelLists of
  EndOfInputBeforeResultReached -> Parsed acc
  ParseError err -> ParseError err
  Parsed output -> Parsed output

takeLispyList: ContinuationParser (PositionMarked Char) LispyList () [LispyList]
takeLispyList continuation input =
 let
  noCloseParenError = errorAts "Matching close parethesis not found for parenthesized block." input
 in
 takeLispyList' [] noCloseParenError continuation input
 
takeLispyList': [LispyList] -> String -> ContinuationParser (PositionMarked Char) LispyList () [LispyList]
takeLispyList' acc closeParenError continuation input =
 input |>
  (take whitespace <| \ _ transition ->
   if | transition == ';' ->
          fastforward 1
       <| take comment
       <| \ _ _ -> takeLispyList' acc closeParenError continuation

      | transition == '\"' ->
          fastforward 1
       <| takeString
       <| \ string _ -> takeLispyList' (acc++[LispyString string]) closeParenError continuation

      | transition == '(' ->
          fastforward 1
       <| takeLispyList
       <| \ list _ -> takeLispyList' (acc++[list]) closeParenError continuation
      | transition == ')' ->
          fastforward 1
       <| continuation (List acc) ()

      | Char.isDigit transition ->
          take number
       <| \ number' _ -> takeLispyList' (acc++[Number number']) closeParenError continuation

      | otherwise -> take
          lispySymbol
       <| \ symbol _ -> takeLispyList' (acc++[Symbol symbol]) closeParenError continuation)
  `markEndOfInputAsError` closeParenError

lispySymbol =
 symbol
  (\c->isWhitespace c || c == ')' || c == '(' || Char.isDigit c) 