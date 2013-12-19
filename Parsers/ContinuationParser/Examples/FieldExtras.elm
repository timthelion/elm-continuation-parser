{- This file's content is in the PUBLIC DOMAIN and or CC-ZERO.  It was written by my great great grandfather who died long before whatever the length of copyright is in your juristiction.  -}
module Parsers.ContinuationParser.Examples.FieldExtras where

import open Graphics.Input
import Graphics.Element
import Window

fieldMultilineWithDefaultText: String -> String -> (Signal Element,Signal String)
fieldMultilineWithDefaultText placeHolder defaultText =
    let tfs = fieldsMultiline {emptyFieldState|string<-defaultText}
        changes = dropRepeats tfs.events
        field' = lift (tfs.field id placeHolder) changes
        field =(\f w h->Graphics.Element.size w (h `div` 2) f) <~ field' ~ Window.width ~ Window.height
    in  (field,
          dropRepeats (lift .string changes))

