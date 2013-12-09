module Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter5 where

import Parsers.ContinuationParser.Examples.LispyListParser as LispyListParser

import Parsers.ContinuationParser.Examples.FieldExtras as FieldExtras

trampolining = [markdown|
## Trampolining

Sadly, we're not quite done yet.  Our lispylist parser works, but if you paste more than a few dozen lines of code into it you'll find you start running into stack overflows!  As of this writing, Elm does not support tail call optimization.

To prevent this, I've provided the continuation parser with the ability to trampoline continuations.  The change we need to make here is quite trivial.

Lets zoom into the second to last case in our `parseTopLevelLispyLists` function.  Rather than writing:

````
      | transition == '(' ->
          fastforward 1 
       <| takeLispyList
           `markEndOfInputAsErrorAt`
          "Matching close parethesis not found for parethesized block"
       <| \ list _ ->
          parseTopLevelLispyLists (acc++[list])
````

We write:

````
      | transition == '(' ->
          fastforward 1 
       <| takeLispyList
           `markEndOfInputAsErrorAt`
          "Matching close parethesis not found for parethesized block"
       <| \ list _ ->
         createSimpleContinautionThunk <|  parseTopLevelLispyLists (acc++[list])
````

The `createSimpleContinuationThunk` function takes a parser and some input and creates an unevaluated thunk.  This thunk then gets passed up the call stack to whatever thunk evaluator is above it.

We could create thunks from every parser in our program.  This would go a long way towards guaranteeing that there would be no stack overflows, but it would be messy and inefficient.  The balance here is to create thunks as often as is needed to prevent stack overflows, but no more often.

Trampolining goes a long way towards preventing stack overflows but it is no silver bullet.  If you remember the `markEndOfInputAsErrorAt` function, this function works as a thunk evaluator.  This means, that we cannot stack `markEndOfInputAsErrorAt`s recursively without risking stack overflow.  In the case of the lispylisp parser, this is thankfully unnecessary.

You can play with the final version of the lispylist parser bellow:
|]

(lispyListParserField,lispyListParserInput) =
 FieldExtras.fieldMultilineWithDefaultText
  "Enter some lisp to be parsed here."
  "(This is (not a list) (but a tree)) \n (\"You see?\" (1 (2 (3 (4 (5.6))))))"

lispyListParserOutput = (\input->asText <| LispyListParser.parseLispyListFile input) <~ lispyListParserInput