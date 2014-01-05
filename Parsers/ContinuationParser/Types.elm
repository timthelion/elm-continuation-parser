module Parsers.ContinuationParser.Types where
{-|
## Types
@docs Parser, ParserResult, Continuation, ContinuationParser
-}

{- internal modules -}
import Lazzy

{- types -}
type Parser input output = [input] -> ParserResult input output

{-| Values of this type exist in two forms, fully evaluated and unevaluated.  When you have a `Parser` which as you see above creates a `ParserResult` this parser result is in an unevaluated form.  You must run it through the `parser` function to get the proper evaluated form.  When a `ParserResult` is in its evaluated form, the `Continue` case is not present. -}
data ParserResult input output
 = Parsed output
 | ParseError String
 | EndOfInputBeforeResultReached
 | Continue {ctype:ContinueType,continuation:Lazzy.Lazy [input] (ParserResult input output)}

data ContinueType
 = Unambiguous
 | EndOfBlock

type Continuation input intermediate output
 = intermediate -> Parser input output

type ContinuationParser input intermediate output
 = (Continuation input intermediate output) -> Parser input output
