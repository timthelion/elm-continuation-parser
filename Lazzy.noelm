{- Unlike the rest of this program, this module is licenced Creative Commons ZERO, PUBLIC DOMAIN, NO COPYRIGHT, written by my great great great grandfather who has been dead for longer than however long copyright lasts in your juristiction. -}

module Lazzy where
{-|
This module provides the ability to turn a function of type `a->b` into a lazy thunk of type `Lazy input output` which can then be evaluated to produce a value of type output.

@docs Lazy, computeLater, evaluate
-}

{-| A lazy thunk -}
type Lazy input output = (input -> output,input)

{-| Create a lazy thunk from a function and its input -}
computeLater: (input -> output) -> input -> Lazy input output
computeLater f input = (f,input)

{-| Evaluate a thunk producing its value -}
evaluate: Lazy input output -> output
evaluate (f,input) = f input

{- NO COPYRIGHT, YOU ARE FREE TO USE THIS CODE! -}