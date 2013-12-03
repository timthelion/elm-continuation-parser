module Parsers.ContinuationParser.PositionMarking where
import open Parsers.ContinuationParser

type PositionMarked char =
 {line: Int
 ,column: Int
 ,char: char}

charsToPositionMarkedChars: [Char] -> [PositionMarked Char]
charsToPositionMarkedChars chars = charsToPositionMarkedChars' chars 0 0
charsToPositionMarkedChars' chars line col =
 case chars of
  (c::cs) -> {line=line,column=col,char=c} ::
    if | c == '\n' -> charsToPositionMarkedChars' cs (line + 1) 0
       | otherwise -> charsToPositionMarkedChars' cs line (col+1)
  [] -> []

handlePositionMarkedInput: LexemeEater char char output -> LexemeEater (PositionMarked char) char output
handlePositionMarkedInput unmarkedLexemeEater =
 (\acc input ->
   let
    unmarkedAcc = map (.char) acc
    unmarkedInput = input.char
   in
   case unmarkedLexemeEater unmarkedAcc unmarkedInput of
    LexemeError err -> LexemeError <| errorAt err input
    EatenLexeme lexeme -> EatenLexeme lexeme
    IncompleteLexeme -> IncompleteLexeme)

parseErrorAt: String -> PositionMarked char -> ParserResult output
parseErrorAt message location = ParseError <| errorAt message location

parseErrorAts: String -> [PositionMarked char] -> ParserResult output
parseErrorAts message input = ParseError <| errorAts message input

errorAt: String -> PositionMarked char -> String
errorAt message location =
 message ++ "\n    On line:"++(show location.line)
         ++ "\n    At column:"++(show location.column)

errorAts: String -> [PositionMarked char] -> String
errorAts message input =
 case input of
  (location::_) -> errorAt message location
  [] -> message ++ "\n    At end of input"

--markEndOfInputAsErrorAt: Parser input output -> String -> Parser input output
markEndOfInputAsErrorAt parser message input =
 case parser input of
  EndOfInputBeforeResultReached ->
   parseErrorAts message input
  otherCases -> otherCases

--markEndOfInputAsError: Parser input output -> String -> Parser input output
markEndOfInputAsError parser message input =
 case parser input of
  EndOfInputBeforeResultReached -> ParseError message
  otherCases -> otherCases