module Parsers.ContinuationParser.Examples.LispyListTutorial.Chapter3 where

import Parsers.ContinuationParser.Examples.ParseUserDataWithLineNumbers as ParseUserDataWithLineNumbers

import Parsers.ContinuationParser.Examples.FieldExtras as FieldExtras

userDataWithLineNumbers = [markdown|

## Adding line numbers to our error messages

Notice that it produces error messages if you don't give it valid input.  We can do better.  These errors are missing line and column markings.  Adding them is easy.  The continuation parser does not in and of itself have any special support for line and column markings.  Instead, we add these markings by anotating our input.  Instead of passing our Parser a list of Chars `[Char]` we pass it a list of PositionMarked Chars `[PositionMarked Char]`.  We now need to modify our LexemeEaters slightly to handle postion marked input.  We do so by passing them through the `handlePositionMarkedInput` function which is of type `handlePositionMarkedInput: LexemeEater [char] opinion output -> LexemeEater [PositionMarked char] opinion output`.

Since I presume that most parsers will want to handle PositionMarked input and provide nice error messages, pre-built LexemeEaters provided by `Parsers.ContinuationParser.Specifics.Lexemes` handle PositionMarked input by default.

Here is the sourcecode to our new parser which prints error messages containing line numbers:
````
import open Parsers.ContinuationParser
import open Parsers.ContinuationParser.LexemeEaters
import open Parsers.ContinuationParser.PositionMarking
import open Parsers.ContinuationParser.Specifics.Lexemes

import String

parseUserData: String -> Either String UserData
parseUserData unparsed
 =
 parse
  (charsToPositionMarkedChars (String.toList unparsed ++ ['\n']))
  parseUserData'

parseUserData': Parser (PositionMarked Char) UserData
parseUserData' =
 take nameField |> \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ name _ ->
 take locationField <| \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ location _ ->
 take occupationField <| \ _ _ ->
 fastforward 1 <|
 take tillEndOfLineUnpadded <| \ occupation _ _ ->
 tillEndOfInput
  (Parsed
   {name=name
   ,location=location
   ,occupation=occupation})
  <| take whitespace
  <| \ _ transition input ->
   parseErrorAts ("Unexpected input "++(show transition)++" near end of file.") input

field: String -> LexemeEater (PositionMarked Char) Char [Char]
field name = handlePositionMarkedInput <| keyword (String.toList name) (==':')
nameField = field "Name"
locationField = field "Location"
occupation = field "Occupation"

tillEndOfLineUnpadded
 =  handlePositionMarkedInput
 <| lexeme (/='\n') (String.trim . String.fromList)
````

So what have we changed here?  Not much:

 - We added a `charsToPositionMarkedChars` to `parseUserData`
 - We changed the type signature of `parseUserData'`
 - We changed the last line of `parseUserData'` to use `parseErrorAts` rather than `ParseError` directly.
   `parseErrorAts: String -> [PositionMarked char] -> ParserResult output` produces a `ParseError` based on the given string and with line column information based on the location of the beginning of the provided input.
 - Finally, we changed the `field` and `tillEndOfLineUnpadded` functions to `handlePositionMarkedInput`

Try out our new changes bellow:
|]

(userDataWithLineNumbersField,userDataWithLineNumbersInput) =
 FieldExtras.fieldMultilineWithDefaultText
  "Enter some UserData to be parsed here."
  "Name: Anne\nLocation: Paris\nOccupation: Truck driver"

userDataWithLineNumbersOutput = (\input->asText <| ParseUserDataWitLineNumbers.parseUserData input) <~ userDataWithLineNumbersInput