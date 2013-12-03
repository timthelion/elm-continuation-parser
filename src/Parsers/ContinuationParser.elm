module Parsers.ContinuationParser where

data ParserResult output
 = Parsed output
 | ParseError String
 | EndOfInputBeforeResultReached

type Parser input output = [input] -> ParserResult output

type Continuation input intermediate opinion output = intermediate -> opinion -> Parser input output

type ContinuationParser input intermediate opinion output = (Continuation input intermediate opinion output) -> Parser input output

type LexemeEater input opinion output = [input] -> input -> EatenLexeme opinion output

data EatenLexeme opinion output
 = EatenLexeme {lexeme: output, transition: opinion}
 | LexemeError String
 | IncompleteLexeme

take: LexemeEater input opinion intermediate -> ContinuationParser input intermediate opinion output
take lexemeEater continuation input = take' [] lexemeEater continuation input
take' acc lexemeEater continuation input =
 case input of
  (i::is) -> case lexemeEater acc i of
              EatenLexeme result -> continuation result.lexeme result.transition (i::is)
              IncompleteLexeme -> take' (acc++[i]) lexemeEater continuation is
              LexemeError err -> ParseError err
  [] -> EndOfInputBeforeResultReached

fastforward: Int -> Parser input output -> [input] -> ParserResult output
fastforward n parser input =
 if | n == 0 -> parser input
    | otherwise ->
       case input of
        (i::is) -> fastforward (n-1) parser is
        [] -> EndOfInputBeforeResultReached