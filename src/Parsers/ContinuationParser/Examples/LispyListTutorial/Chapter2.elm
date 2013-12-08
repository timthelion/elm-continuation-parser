module Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter2 where

import Parsers.ContinuationParser.Examples.ParseUserDataWithoutLineNumbers as ParseUserDataWithoutLineNumbers

import Parsers.ContinuationParser.Examples.FieldExtras as FieldExtras

userDataWithoutLineNumbers = [markdown|
# Our first parser

Say we have the String:
````
Name: Timothy Hobbs
Location: Prague
Ocupation: Programmer
````
And we want to put it into a record of  `type UserData = {name:String,location:String,occupation:String}`

### What is our parser going to do?

 - Consume the `Name:` field
 - Consume anything following the `Name:` marker right up till the end of that line
 - Convert that to a string
 - Trim off the whitespace
 - Use that trimmed string as our first intermediate value.
 - Repeat for `Location:` and `Occupation:`
 - Consume whitespace untill we reach the end of input, throwing an error if we find any garbage at the end of the file.

#### Basic imports

We'll start by importing some modules from the continuation-parser library:

````
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.CharacterClassification -- (isWhitespace)

import String
````
The ContinuationParser module provides the basic functions and data types that we'll need to do our parsing.  The LexemeEaters module provides basic functions for constructing new LexemeEaters. We also import String, because our parser will work with lists of characters rather than Elm Strings and so we'll need to convert between the two.

#### Exceptions

Our parser should report errors when there is unexpected input.  It should also report an error if it reaches the end of the file before it is done reading in the Name Location and Occupation that it was looking for.  One special case is that the line in which the occupation is listed does not necessarily need to end with a newline.  There could be an end of input there instead.  This can be easilly worked around in several ways.  We could provide a special case for dealing with the last entry. I think it is more elegant simply to append an extra newline to the end of the input.

#### Our main function

````
parseUserData: String -> ParserResult Char UserData
parseUserData unparsed =
 parse (String.toList unparsed ++ ['\n']) parseUserData'
````

First we prepare our input, converting it from a String to a [Char] and then appending the newline.  Then we pass it on to a Parser:

#### Our Parser

````
parseUserData': Parser Char UserData
parseUserData' =
 take nameField <| \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ name _ ->
 fastforward 1 <|
 take locationField <| \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ location _ ->
 fastforward 1 <|
 take occupationField <| \ _ _ -> 
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ occupation _ ->
 tillEndOfInput
  (Parsed
   {name = name
   ,location = location
   ,occupation = occupation
   })
  <| take whitespace
  <| \ _ transition _ ->
    ParseError ("Unexpected input "++(show transition)++" near end of file.")
````

Here we `take` things in the order that they should appear in the file.  That weird `<| \ _ _ ->` notation is just us passing a lamda which defines a Continuation.  It is similar to the syntax we see in Philip Wadler's 1992 paper ["The Essence of Functional Programming."](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html).

PS: That paper is a good read for those who might be looking at doing monad like stuff in Elm, as all of it's examples are written without taking advantage of typeclasses or do-notation.  For the same reason, it is a good read for anyone just generally confused about monads.

````
take foo <| \ a b ->
take bar <| \ c d ->
(\ input -> Parsed (a,c))
````

 ===

````
take foo (\ a b ->
take bar (\ c d ->
(\ input -> Parsed (a,c))))
````

 ~=

````
do
 (a,b) <- take foo
 (c,d) <- take bar
 return (Parsed (a,c))
````

#### Our LexemeEaters

I've yet to define several LexemeEaters; `nameField`,`locationField`, and `occupationField` are defined bellow as:

````
field: String -> LexemeEater Char Char [Char]
field name = keyword (String.toList name) (\c->c==':')
nameField = field "Name"
locationField = field "Location"
occupationField = field "Occupation"
````

The `keyword` function takes a list representing the keyword to be eaten and a "punctuation test".  The punctuation test is used to determine if the lexeme being eaten has been eaten in its entirety.  Take for example the keyword "as". If we didn't test for punctuation at the end of this keyword then our LexemeEater would match the word "assumtion".  The `keyword` function produces a LexemeEater which demands that all keywords be followed by some form of punctiation, be it whitespace or a symbol.  Here, we define punctuation as being the character ':'.  Keyword LexemeEaters do not consume the punctuation markings that follow the lexeme, and this is why we need to `fastforward` past the ':' for each feild marker.

We still have one last LexemeEater to define and that is:
````
tillEndOfLineUnpadded = lexeme (\c->c/='\n') (String.trim . String.fromList)
````
This LexemeEater is built with the `lexeme` function. The `lexeme` function takes two functions as arguments `test: char -> Bool` and `conversion: [char] -> output`.  The LexemeEater produced by `lexeme` will eat so long as `test` is positive for the current next character.  As a value it produces the eaten input as passed through the `conversion` function.  Our conversion function turns it to a string and removes whitespace from either side.

Finally, the `whitespace` LexemeEater used in this example is the same as the one defined near the beginning of this tutorial.

You can play with this parser bellow ([Source](https://github.com/timthelion/elm-continuation-parser/blob/master/src/Parsers/ContinuationParser/Examples/ParseUserDataWithoutLineNumbers.elm)):
|]

(userDataWithoutLineNumbersField,userDataWithoutLineNumbersInput) =
 FieldExtras.fieldMultilineWithDefaultText
  "Enter some UserData to be parsed here."
  "Name: Suzana\nLocation: Rome\nOccupation: System Architect"

userDataWithoutLineNumbersOutput = (\input->asText <| ParseUserDataWithoutLineNumbers.parseUserData input) <~ userDataWithoutLineNumbersInput