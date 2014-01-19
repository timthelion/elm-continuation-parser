{-
Copyright information at end of file.
-}
module List.CPSExtras where
{-|
This module contains generic helper functions for manipulating lists.

@docs isPrefixOf, isSuffixOf

-}
isPrefixOf: [a] -> [a] -> Bool
isPrefixOf prefix list =
 case (prefix,list) of
  ((p::ps),(l::ls)) ->
   if | p == l -> isPrefixOf ps ls
      | otherwise -> False
  ((p::ps),[]) -> False
  ([],(l::ls)) -> True
  ([],[])      -> True

isSuffixOf: [a] -> [a] -> Bool
isSuffixOf suffix list =
 isPrefixOf (reverse suffix) (reverse list)
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