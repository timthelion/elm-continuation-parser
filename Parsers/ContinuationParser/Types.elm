module Parsers.ContinuationParser.Types where
{-|
## Types
@docs Parser, ParserResult, Continuation, ContinuationParser
-}

{- internal modules -}
import Lazzy

{- types -}
type Parser input output = [input] -> ParserResult input output

{-| This is an internal result.  For final parser results(without the Continue case) see the Parsers.ContinuationParser.FinalParserResult module. -}
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
