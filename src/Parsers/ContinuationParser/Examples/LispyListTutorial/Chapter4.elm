module  Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter4 where

import Parsers.ContinuationParser.Examples.LispyListParserWithoutTrampolining as LispyListParserWithoutTrampolining

import Parsers.ContinuationParser.Examples.FieldExtras as FieldExtras

parsingLispyLists = [markdown|
## Parsing LISP

While parsing user data is good fun, I still haven't demonstrated that the `elm-continuation-parser` can be used for "real work".  Why don't we parse something more interesting. Say LISP.  I choose lisp as my test case for parsing libraries because its syntax is simple and unambiguous, yet parsing it requires most of the same features used when parsing more syntactically complex languages such as Haskell or Elm.

While our last example looked like a single block of somewhat ugly do-notation, this example is going to require multiple functions and recursion.

### Input

Take a short snippet of lisp code:

````
(hello world (this is a test) 1 2 3 4.5)
````
In the world of lisp, this is a hetrogeneous list containing 7 atoms:

 - 2 symbols
 - 1 list
 - 4 numbers

The list:

````
("Hello there this is a test.")
````

Contains just one atom, a single string.

It is also important to note, that a lisp file such as

````

Hello bar baz (boo) 23

````

is invalid, becuse not all of the atoms are contained within a list.

### Our output

````
data LispyList
 = List [LispyList]
 | Symbol String
 | Number Float
 | LispyString String
````

### The tasks of the LispyList parser

This leaves our lispylist parser the following tasks:

 - Consume input that is outside a lispy list, throwing errors unless it is whitespace, a comment or the beginning of a lispy list.
 - Ignore everything that falls between a ';' and a '\n' as a comment, unless the ';' falls within a quoted String.
 - Consume top level lispy lists
  + Consume comments
  + Strings
  + Numbers
  + Symbols
  + Embeded lispy Lists
  + All while ignoring comments

### Imports

The imports are much the same as with our earlier example:

````
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.Specifics.Lexemes

import Char
import String
import List
````

However there are two continuation-parser module's that I did not talk about previously:

````
import open Parsers.ContinuationParser.Specifics.ContinuationParsers
import open Parsers.CharacterClassification
````

The `ContinuationParsers` module is intended to provide generic `ContinuationParser`s.  As of this writing it includes only one and that is the `takeString` parser.  This takes a quoted string, dealing with standard backslashed escape sequences.

The `CharsetClassification` module provides functions like `isWhitespace` and `isLetter` that didn't make their way into the standard `Char` library.

### Our main function
Our main function looks much like the main function in the previous example.

```
parseLispyListFile: String -> ParserResult (PositionMarked Char) [LispyList]
parseLispyListFile input
 = parse
    (charsToPositionMarkedChars <| String.toList input)
    (parseTopLevelLispyLists [])
````

The only interesting thing here, is that we are passing an empty accumulator to `parseTopLevelLispyLists`.

**Our next function** is much more interesting.

In `parsec`'s terms, this function would probably be something along the lines of:

````
do
 lispyLists <- many  $ do { _ <- whitespaceAndComments; lispyList}
 _ <- whitespaceAndComments
 eof
 return lispyLists
````

This is what one might call failiure driven parsing.  It goes forward consuming lispy lists untill something hangs up, then it looks at what else it might try (in this case consuming whitespace and comments up untill the end of the file).


Instead of enumerating failiure cases, we try to make an educated guess on what comes next.

If we recall the type of a  `ContinuationParser`

````
type ContinuationParser input intermediate opinion output
 =  (Continuation input intermediate opinion output)
 -> Parser input output

type Continuation input intermediate opinion output
 = intermediate -> opinion -> Parser input output
````

We see that `ContinuationParser`s not only pass their `Continuation`s an intermediate value, they also pass an argument named `opinion`.  In the case of most `ContinuationParser`s built with the `take` function, the `opinion` is the next character in the input after the lexeme which has just been consumed.  In the elm-continuation-parser source code, I alternate between calling the `opinion` argument "opinion" and "transition".  This is because the `opinion` is usually the character at the transition of two lexemes but I want to leave open the possibility for users of my library to provide a different type of opinion on what comes next.

````
parseTopLevelLispyLists:
    [LispyList]
 -> Parser (PositionMarked Char) [LispyList]
parseTopLevelLispyLists acc =
      takeWithFallbackValue whitespace (Parsed acc)
   <| \ _ transition ->
   if | transition == ';' ->
           fastforward 1
        <| takeWithFallbackValue comment (Parsed acc)
        <| \ _ _ -> parseTopLevelLispyLists acc

      | transition == '(' ->
           fastforward 1
        <| takeLispyList
            `markEndOfInputAsErrorAt`
           "Matching close parenthesis not found for parenthesized block."
        <| \ list _ -> parseTopLevelLispyLists (acc++[list])

      | otherwise ->
         (\input -> parseErrorAts ("Unexpected input:" ++ (show transition)) input)
````

In this excerpt we take some `whitespace` and bind the first non-whitespace character in our input to the name `transition`.  We then make a choice:

 - If the `transition` character is a semicolon ';', we `fastforward` past that semicolon, and read till the end of the line(the comment lexeme reads untill it reaches a newline character).  We then ignore any intermediate value that the `comment` lexeme created(the comment's text) and go on to run `parseTopLevelLists` on the remainder of the input.  If our file consisted mearly of comments and whitespace then we would continue looping like this untill the end of the input.

 - If the `transition` character is '(', then we again `fastforward` past the '(' and then we call the `takeLispyList` function.  The `takeLispyList` function provides a `LispyList` as its intermediate value. The whole point of our parser is to convert a `String` to a `LispyList`. We now have a `LispyList` which we add to `acc`.  After we do so, we continue looking for more top level lispy lists.

 - If the transition character is neither a semicolon nor an open parenthesis we return an error.

When we take `whitespace` or a `comment`, we do not just `take` that input, we `takeWithFallbackValue` it.  The `takeWithFallbackValue` function tries to take a lexeme, but if it reaches the end of the input before the lexeme is complete, it returns a given fallback value instead.  `takeWithFallbackValue whitespace (parsed acc) continuation` means that if the input only of whitespace up till the end of the input then we return (Parsed acc), however, if the whitespace lexeme "completes"(if we find something other than whitespace) then we go on to the continuation.  Here, if the only thing left in our file is whitespace or a comment, we are done and we return the accumulated `[LispyLists]`.

The `markEndOfInputAsErrorAt` function takes a ContinuationParser and an error message and returns a ContinuationParser which evaluates to the error message(with appropriate line numbers) if the end of input is reached before the continuation is reached.  In this case, if the end of input is reached before the lispylist has finished being taken, a close paren error is displayed.

**The `takeLispyList` function** is the workhorse of our parser:

````
takeLispyList: ContinuationParser (PositionMarked Char) LispyList Char [LispyList]
takeLispyList continuation = takeLispyList' [] continuation

takeLispyList':
    [LispyList]
 -> ContinuationParser (PositionMarked Char) LispyList Char [LispyList]
takeLispyList' acc continuation =
 take whitespace <| \ _ transition ->
 if | transition == ';' ->
       fastforward 1
    <| take comment
    <| \ _ _ -> takeLispyList' acc continuation

    | transition == '\"' ->
        fastforward 1
     <| takeString
     <| \ string _ -> takeLispyList' (acc++[LispyString string]) continuation

    | transition == '(' ->
        fastforward 1
     <| takeLispyList
     <| \ list _ -> takeLispyList' (acc++[list]) continuation

    | transition == ')' ->
        fastforward 1
     <| continuation (List acc) ')'

    | Char.isDigit transition ->
        take float
     <| \ number' _ -> takeLispyList' (acc ++ [Number number']) continuation

    | otherwise ->
        take lispySymbol
     <| \ symbol _ -> takeLispyList' (acc++[Symbol]) continuation
````

There is surprisingly little new here.   The only thing of note in this function are the last two cases: isDigit and otherwise.  There is no fastforward here, because the lexeme boundary IS the start of the next lexeme.

Our last function in the LispyList parser is

````
lispySymbol =
 symbol
  (\c->isWhitespace c || c == ')' || c == '(' || Char.isDigit c)
````

You can try out our new LispyList parser bellow ([Source](https://github.com/timthelion/elm-continuation-parser/blob/master/src/Parsers/ContinuationParser/Examples/LispyListParserWithoutTrampolining.elm)):
|]

(lispyListParserWithoutTrampoliningField,lispyListParserWithoutTrampoliningInput) =
 FieldExtras.fieldMultilineWithDefaultText
  "Enter some lisp to be parsed here."
  "(This is (not a list) (but a tree)) \n (\"You see?\" (1 (2 (3 (4 (5.6))))))"

lispyListParserWithoutTrampoliningOutput = (\input->asText <| LispyListParserWithoutTrampolining.parseLispyListFile input) <~ lispyListParserWithoutTrampoliningInput