module Main exposing (..)

import Html
import String


isExceedingLimit limit str =
    (String.length str) > limit


convertToUpperCase str =
    String.toUpper str


checkAndConvert str =
    if isExceedingLimit 10 str then
        convertToUpperCase str
    else
        str


main =
    Html.text (checkAndConvert "This is text more than 10 characters")
