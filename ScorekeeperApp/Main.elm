module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

-- Model --

type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }

type alias Player =
    { id : Int
    , name : String
    , points : Int
    }

type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }

initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }

-- Update --

type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play
    | Remove Player


update : Msg -> Model -> Model
update msg model =
    case msg of
        Edit player ->
            { model
                | playerId = Just player.id
                , name = player.name
            }

        Input name ->
            { model | name = name }

        Save ->
          if (String.isEmpty model.name) then
              model
          else
              save model

        Cancel ->
            { model
                | name = ""
                , playerId = Nothing
            }

        Score player points ->
            scorePoint model player points

        DeletePlay play ->
            deletePlay play model

        Remove player ->
            removePlayer player model


removePlayer : Player -> Model -> Model
removePlayer player model =
    let
        newPlayers =
            List.filter (\p -> p.id /= player.id) model.players

        newPlays =
            List.filter (\p -> p.playerId /= player.id) model.plays

    in
        { model
            | players = newPlayers
            , plays = newPlays
        }


deletePlay : Play -> Model -> Model
deletePlay play model =
    let
        newPlayers =
            List.map
                (\player ->
                      if player.id == play.playerId then
                          { player
                              | points = player.points - play.points
                          }
                      else
                          player

                ) model.players

        newPlays =
            List.filter (\ple -> ple.id /= play.id) model.plays
    in
        { model
            | plays = newPlays
            , players = newPlayers
        }



scorePoint : Model -> Player -> Int -> Model
scorePoint model player points =
    let
        play =
            { id = (List.length model.plays)
            , playerId = player.id
            , name = player.name
            , points = points
            }

        newPlayers =
            List.map
                (\playr ->
                      if playr.id == player.id then
                          { playr
                              | points = playr.points + points
                          }
                      else
                          playr
                ) model.players

        newPlays =
            play :: model.plays
    in
        { model
            | plays = newPlays
            , players = newPlayers
        }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id
        Nothing ->
            add model


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
              List.map (\player -> (editPlayer player id model.name)) model.players
    in
        { model
            | players = newPlayers
            , name = ""
            , playerId = Nothing
        }


editPlayer : Player -> Int -> String -> Player
editPlayer player id name =
        if player.id == id then
            { player
                | name = name
            }
        else
            player


add : Model -> Model
add model =
    let
        player =
            { id = (List.length model.players)
            , name = model.name
            , points = 0
            }

        newPlayers =
            player :: model.players

    in
        { model
            | name = ""
            , players = newPlayers
        }


-- View --

view : Model -> Html Msg
view model =
  div [ class "scoreboard" ]
      [ h1 [] [ text "Score Keeper" ]
      , playersContainer model
      , playerForm model
      , playContainer model
      , p [] [ text (toString model) ]
      ]

playersContainer : Model -> Html Msg
playersContainer model =
    div []
        [ playerListHeader
        , playerList model
        ]

playContainer : Model -> Html Msg
playContainer model =
    div []
        [ playListHeader
        , playList model
        ]

playerListHeader : Html Msg
playerListHeader =
    header []
           [ div [] [ text("Name") ]
           , div [] [ text("Points") ]
           ]

playListHeader : Html Msg
playListHeader =
    header []
           [ div [] [ text("Name") ]
           , div [] [ text("Points") ]
           ]

playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.map (player model.playerId)
        |> ul[]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map (play)
        |> ul []

play : Play -> Html Msg
play play =
    li []
       [ i [ class "remove", onClick (DeletePlay play) ] []
       , div [] [ text play.name ]
       , div [] [ text (toString play.points) ]
       ]

playerEditClass : Maybe Int -> Player -> String
playerEditClass playerEditId player =
    case playerEditId of
        Just id ->
            if player.id == id then
                "edit"
            else
                ""

        Nothing ->
            ""

player : Maybe Int -> Player -> Html Msg
player playerEditId player =
    li []
       [  i
            [ class "remove", onClick (Remove player) ]
            []
        , i
            [ class "edit", onClick (Edit player) ]
            []
        , div
            [ class (playerEditClass playerEditId player) ]
            [ text(player.name) ]
        , button
            [ type_ "button", onClick (Score player 2) ]
            [ text("2 Pts.") ]
        , button
            [ type_ "button", onClick (Score player 3) ]
            [ text("3 Pts.") ]
        , div
            []
            [ text(toString player.points) ]
       ]

playerForm : Model -> Html Msg
playerForm model =
  Html.form [ onSubmit Save ]
            [ input
                [ type_ "text"
                , placeholder "Add/Edit Player..."
                , onInput Input
                , value model.name
                ]
                []
            , button [ type_ "submit" ] [ text ("Save") ]
            , button [ type_ "button", onClick Cancel ] [ text ("Cancel") ]
            ]

main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
