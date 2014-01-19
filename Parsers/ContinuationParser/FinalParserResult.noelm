{-
Copyright information can be found at the end of this file.
-}
module Parsers.ContinuationParser.FinalParserResult where
{-|
This module provides the FinalParserResult datatype. Which cannot live in the Parsers.ContinuationParser.Types module because its constructors conflict with ParserResult.

@docs FinalParserResult
-}
data FinalParserResult output
 = Parsed output
 | ParseError String
 | EndOfInputBeforeResultReached
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