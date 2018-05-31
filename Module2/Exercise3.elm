module Main exposing (..)

import Html
import String

wordCount = String.words >> List.length

main = Html.text(toString ( wordCount "This is a test String"))
