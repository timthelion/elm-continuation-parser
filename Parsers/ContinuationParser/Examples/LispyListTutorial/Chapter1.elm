module Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter1 where

basicTypes = [markdown|
# The basic types

The continuation parser is a library which helps you create Parsers.  It takes inspiration from the Haskell [Parsec](http://hackage.haskell.org/package/parsec) library in that these parsers consist of strings of "input consumers" which produce intermediate values.

The basic parser type is:
````
type Parser input output = [input] -> ParserResult input output
````
where
````
data ParserResult input output
 = Parsed output
 | ParseError String
 | EndOfInputBeforeResultReached
 | Continue {id:String,continuation:Lazy [input] (ParserResult input output)}
````
Note: For now, do not pay attention to the "Continue" case. It is not strictly necessary. Its only purpose is [trampolining](http://stackoverflow.com/questions/189725/what-is-a-trampoline-function).

Unlike parsec, the continuation parser is written in continuation passing style rather than monadic style.

Rather than stringing together ParsecTs with the monadic bind we string together Continuations. Continuations are just functions which take yet to be consumed input.  There is no universal `bind` or `>>=`.  Rather any function of type ContinuationParser can be passed a continuation.

````
type ContinuationParser input intermediate opinion output
 = (Continuation input intermediate opinion output) -> Parser input output
````
ContinuationParsers always:

 - Consume some input
 - Produce an intermediate value
  - Call a continuation funciton
    + Passing it the intermediate value
    + Passing it an opinion on what comes next(usually the next character of the input).
 - Return the value of the continuation

````
type Continuation input intermediate opinion output
 = intermediate -> opinion -> Parser input output
````
A Continuation is just a function which takes an intermediate value, an "opinion" on what probably comes next, and some input. It produces a ParserResult.

The most commonly used type of ContinuationParsers is produced by the `take` function.

````
take: LexemEater input opinion intermediate 
    -> ContinuationParser input intermediate opinion output
````

````
type LexemeEater input opinion output
 = [input] -> input -> EatenLexeme opinion output

data EatenLexeme opinion output
 = EatenLexeme {lexeme: output, transition: opinion}
 | LexemeError String
 | IncompleteLexeme
````
Here is an example LexemeEater:

````
whitespace: LexemeEater Char Char [Char]
whitespace consumed input =
 if | isWhitespace input -> IncompleteLexeme
    | otherwise -> EatenLexeme {lexeme=consumed,transition=input}
````
This tells `take` that while the input(the next character in the input stream) isWhitespace, continue consuming input.  Once we see that the next character in the input stream is NOT whitespace, pass on the whitespace we just consumed to the Continuation as an intermediate value.

Of course, there was a bit of boiler-plate in there.  Rather than writing that code out, we can write the `whitespace` LexemeEater using the `charset` function in `Parsers.ContinuationParser.LexemeEaters`:

````
whitespace = charset isWhitespace
````

**Finally** I'd like to point out that `Parser`s are run on input using the `parse` function:

````
parse: [input] -> Parser input output -> ParserResult input output
````

While from the standpoint of types this may not seem entirely necessary, in reality it is.  This function evaluates trampolined thunks passed down with the "Continue" case.  The output of this function is therefore never "Continue ..." but one of the more usefull results.

|]