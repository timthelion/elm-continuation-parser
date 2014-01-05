{-
For copyright information see the COPYING file or the end of this file.
-}
module Parsers.ContinuationParser where
{-|
This module provides functions which operate on Parsers and ContinuationParsers(to be found int the Parsers.ContinuationParser.Types module).

# Fundimentals
@docs parse, return, <|>

# Seeking
@docs fastforward

# Errors and end of input
@docs tillEndOfInput

# Continues
@docs continue, evaluateContinues, evaluateContinuesTillEndOfBlock
-}
{- base imports -}
import Trampoline
import Either

{- internal modules -}
import Lazzy
import Parsers.ContinuationParser.FinalParserResult as FinalParserResult
import open Parsers.ContinuationParser.Types


{-| This function should be run on any `Parser` from which you would like to create a usable result.  Think of running a parser with the `parse` function line running a monad.
-}
parse: [input] -> Parser input output -> FinalParserResult.FinalParserResult output
parse input parser =
 evaluateContinues (parser input) |> \ result -> 
 case result of
  EndOfInputBeforeResultReached -> FinalParserResult.EndOfInputBeforeResultReached
  ParseError err -> FinalParserResult.ParseError err
  Parsed output -> FinalParserResult.Parsed output

{-| Parse till end of input, when end of input is reached return the given ParserResult.  Good for error checks. -}
tillEndOfInput: ParserResult input output -> Parser input ignoredOutput -> Parser input output
tillEndOfInput result parser input =
 case evaluateContinues <| parser input of
  EndOfInputBeforeResultReached -> result
  ParseError err -> ParseError err
  Parsed _ -> {- This shouldn't happen -} ParseError "Programmer error: End of input parsers should not return a result."

replaceEndOfInputWith: ContinuationParser input intermediate output -> ParserResult input output -> ContinuationParser input intermediate output
replaceEndOfInputWith continuationParser result continuation input =
 case evaluateContinuesTillEndOfBlock <| continuationParser (markAsEndOfBlock continuation) input of
  EndOfInputBeforeResultReached -> result
  Continue value -> Lazzy.evaluate value.continuation -- Evaluate end of block marker
  otherCases -> otherCases

markAsEndOfBlock: Continuation input intermediate output -> Continuation input intermediate output
markAsEndOfBlock continuation intermediate = continue EndOfBlock <| continuation intermediate

{-| Create a parser which ignores its input and returns the given result directly. -}
return: ParserResult input output -> Parser input output
return result _ = result

{-| Remove a n characters/tokens from the input -}
fastforward: Int -> Parser input output -> Parser input output
fastforward n parser input =
 if | n == 0 -> parser input
    | otherwise ->
       case input of
        (i::is) -> fastforward (n-1) parser is
        [] -> EndOfInputBeforeResultReached




infixl 0 <|>
{-|
This is the standard parser choice operator.  It alows you to say:
 Try the parser on the left,
 and if that fails try the one on the right.

It is a little bit more flexible than parsec's <|>.
Whereas in parsec <|> is LL(1) by default in the elm continuation parser it is LL(k).  The elm continuation parser is satisfied that the choice is not ambiguous when a parser returns Continue.

Notes:
 - take returns Continue at the end of each lexeme by default, so the choice operator is satisfied after a lexeme is eaten.
 - You can do something similar to `try` by using evaluateContinuesTillEndOfBlock along with markAsEndOfBlock to prevent <|> from believing that the choice is unambiguous.
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

{- Continues -}
{-| Create a Continue of the given type -}
continue: ContinueType -> Parser input output -> [input] -> ParserResult input output
continue ctype parser input =
 Continue
  {ctype=ctype
  ,continuation = Lazzy.computeLater parser input}

{-| Evaluate all Continues returning a fully evaluated ParserResult -}
evaluateContinues: ParserResult input output -> ParserResult input output
evaluateContinues result =
 Trampoline.trampoline result <| \ result ->
 case result of
  Continue value ->  Either.Left <| Lazzy.evaluate value.continuation
  _ -> Either.Right result

{-| Evaluate all Continues untill an EndOfBlock is reached. Then return it, unevaluated. -}
evaluateContinuesTillEndOfBlock: ParserResult input output -> ParserResult input output
evaluateContinuesTillEndOfBlock result =
 Trampoline.trampoline result <| \ result ->
 case result of
  Continue value ->
   if | value.ctype == EndOfBlock -> Either.Right <| Continue value
      | otherwise -> Either.Left <| Lazzy.evaluate value.continuation
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