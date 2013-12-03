module Parsers.ContinuationParser.VariousParsersAndLexemeEaters where
import String
import Char

import open Parsers.CharacterClassification
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking

--positionMarkedCharset: (char -> Bool) -> LexemeEater (PositionMarked char) char [char]
positionMarkedCharset test = handlePositionMarkedInput <| charset test

--whitespace: LexemeEater (PositionMarked Char) Char [Char]
whitespace = positionMarkedCharset isWhitespace

--comment: LexemeEater (PositionMarked Char) Char [Char]
comment = positionMarkedCharset (\c->c/='\n')

--number: LexemeEater (PositionMarked Char) Char Float
number = handlePositionMarkedInput <| lexemeMaybe (\c->Char.isDigit c||c=='.') (String.toFloat . String.fromList)

--symbol: (Char -> Bool) -> LexemeEater (PositionMarked Char) Char String
symbol punctuationTest = handlePositionMarkedInput (symbol' punctuationTest)
symbol' punctuationTest = lexeme (not . punctuationTest) String.fromList

--takeString: ContinuationParser (PositionMarked Char) String () output
takeString continuation = takeString' [] continuation `markEndOfInputAsErrorAt` "Matching quote not found for string."
--takeString': [Char] -> ContinuationParser (PositionMarked Char) String () output
takeString' acc continuation input =
 input |>
  (take normalStringSegment <| \ segment transition ->
   if | transition == '\\' ->
          fastforward 1
       <| take escapedChar
       <| \ escaped _ -> fastforward 1 <| takeString' (acc++ segment ++ [escaped]) continuation

      | transition == '\"' -> fastforward 1 <| continuation (String.fromList <| acc ++ segment) ())

normalStringSegment: LexemeEater (PositionMarked Char) Char [Char]
normalStringSegment = positionMarkedCharset (\c-> c/='\"' && c/= '\\')

--escapedChar: LexemeEater (PositionMarked Char) () Char
escapedChar acc input =
 let
  output =
   if | input.char == 't' -> '\t'
      | input.char == 'n' -> '\n'
      | input.char == 'r' -> '\r'
      | input.char == '0' -> '\0'
      | otherwise -> input.char
 in EatenLexeme {lexeme=output,transition=()}