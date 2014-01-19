{-
Copyright information can be found at the end of this file.
-}
module Parsers.ContinuationParser.Types where
{-|
## Types
@docs Parser, ParserResult, Continuation, ContinuationParser, Error
-}

{- internal modules -}
import Lazzy

{- types -}
type Parser input output = [input] -> ParserResult input output

{-| This is an internal result.  For final parser results(without the Continue case) see the Parsers.ContinuationParser.FinalParserResult module. -}
data ParserResult input output
 = Parsed output
 | ParseError Error
 | EndOfInputBeforeResultReached Expectation
 | Continue {ctype:ContinueType,continuation:Lazzy.Lazy [input] (ParserResult input output)}

data ContinueType
 = Unambiguous
 | EndOfBlock

type Error = {message: String
             ,expected: Expectation}

type Expectation = Maybe String

type Continuation input intermediate output
 = intermediate -> Parser input output

type ContinuationParser input intermediate output
 = (Continuation input intermediate output) -> Parser input output
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