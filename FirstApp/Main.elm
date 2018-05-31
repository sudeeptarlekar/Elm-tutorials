module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


 -- Model --
type alias Model
    = { calories: Int
      , input: Int
      , error: Maybe String
      }

initModel : Model
initModel =
    {
      calories = 0,
      input = 0,
      error = Nothing
    }



-- Update --
type Msg
    = AddCalorie
    | InsertCalorie String
    | Clear

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCalorie ->
            { model
                | calories = model.calories + model.input,
                  input = 0
            }
        InsertCalorie value ->
            case String.toInt value of
                Ok inputValue ->
                    { model
                        | input = inputValue,
                          error = Nothing
                    }
                Err error ->
                    {
                      model
                        | input = 0,
                          error = Just error
                    }
        Clear ->
            initModel



-- View --
view model =
    div []
        [
          h3 [] [ text("Total Calories: " ++ (toString model.calories)) ],
          input
            [
              type_ "text",
              onInput InsertCalorie,
              value
                (
                  if model.input == 0 then
                    ""
                  else
                    toString model.input
                )
            ] [],
          div [] [ text (Maybe.withDefault "" model.error) ],
          button [ type_ "button", onClick AddCalorie ] [ text "Add" ],
          button [ type_ "button", onClick Clear ] [ text "Clear" ]
        ]


-- Did not understand this annotation
main : Program Never Model Msg
main = Html.beginnerProgram
        {
          model = initModel,
          update = update,
          view = view
        }


