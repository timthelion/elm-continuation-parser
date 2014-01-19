module Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter4 where

import Parsers.ContinuationParser.Examples.LispyListParser as LispyListParser

import Parsers.ContinuationParser.Examples.FieldExtras as FieldExtras

trampolining = [markdown|
## Trampolining

Sadly, we're not quite done yet.  Our lispylist parser works, but if you paste a parenthesized block with more than a few dozen lines of code into it you'll find you start running into stack overflows!  As of this writing, Elm does not support tail call optimization.

Luckly, the elm-continuation-parser has good support for trampolining, a special method for clearing out the call stack.  Some trampolining is happening for you automatically.  For example, every time we take a top level lispy list we put our code through the `markEndOfInputAsErrorAt` function.  This function always trampolines its continuations.  We're already even better off than that, since the `takeString` function happens to call `markEndOfInputAsErrorAt`.

However, if we want to support large quoteless parenthesized blocks, we need to do some more fiddling.  Luckly, trampolining is pretty simple:

The `createSimpleContinuationThunk` function takes a parser and some input and creates an unevaluated thunk.  This thunk then gets passed up the call stack to whatever thunk evaluator is above it.  As it gets passed up, the call stack is cleared, thus allowing us to do more recursion without stack overflow errors.

We could create thunks from every parser in our program.  This would go a long way towards guaranteeing that there would be no stack overflows but it would also be messy and inefficient.  The balance here is to create thunks as often as is needed to prevent stack overflows, but no more often.

As an example, I chose to run the `createSimpleContinuationThunk` every time the `takeLispyList'` function reaches the end of a lispylist.  

The change is extremely trivial:

````
      | transition == ')' ->
          fastforward 1
       <| continuation (List acc) ')'
````

becomes

````
      | transition == ')' ->
          fastforward 1
       <| createSimpleContinuationThunk <| continuation (List acc) ')'
````

#### Thunk evaluators

Trampolining goes a long way towards preventing stack overflows but it is no silver bullet.  If you remember the `markEndOfInputAsErrorAt` function, this function works as a thunk evaluator.  When a thunk is created in place of a continuation, it is passed up the call stack untill it reaches the first thunk evaluator.  This means, that we cannot stack `markEndOfInputAsErrorAt`s recursively without risking stack overflow.  In the case of the lispylisp parser, this is thankfully unnecessary.

The exact semantics are a bit complicated.  The `markEndOfInputAsErrorAt` function evaluates thunks using the `evauateContinuationsTill: String -> ParserResult input output -> ParserResult input output` function which takes a `String` id.  This evaluates every thunk who's id is not `id`.  `markEndOfInputAsErrorAt` then turns it's continuation into a thunk with the id it passed to `evaluateContinuationsTill`.  `markEndOfInputAsErrorAt` uses the error message including line numbers as it's final thunk id.  Using a `String` as an Id is not good practice however, and were I to create the continuation parser from scratch I might pass a GUID generator along with the input so we could do this more cleanly.  The hope is, however, that in the future Elm will support tail call optimization and all this manual trampolining code will become unecessary.  

You can play with the final version of the lispylist parser bellow:
|]

(lispyListParserField,lispyListParserInput) =
 FieldExtras.fieldMultilineWithDefaultText
  "Enter some lisp to be parsed here."
  "(This is (not a list) (but a tree)) \n (\"You see?\" (1 (2 (3 (4 (5.6))))))"

lispyListParserOutput = (\input->asText <| LispyListParser.parseLispyListFile input) <~ lispyListParserInput