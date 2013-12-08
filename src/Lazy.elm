{- Unlike the rest of this program, this module is licenced Creative Commons ZERO, PUBLIC DOMAIN, NO COPYRIGHT, written by my great great great grandfather who has been dead for longer than however long copyright lasts in your juristiction. -}

module Lazy where

type Lazy input output = (input->output,input)

--evaluate: Lazy input output -> output
evaluate (f,input) = f input

--computeLater: (input->output) -> input -> Lazy input output
computeLater f input = (f,input)

{- NO COPYRIGHT, YOU ARE FREE TO USE THIS CODE! -}