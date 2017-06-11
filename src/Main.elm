module Main exposing (..)

import Game exposing (Msg)
import Html exposing (Html, programWithFlags, h1, h5, div, map, a, text)
import Html.Attributes exposing (href)
import Svg.Attributes exposing (..)
import Random exposing (Seed, initialSeed)
import FontAwesome as Icon
import Color


type alias Model =
    { game : Game.Model }


type alias Flags =
    { randomSeed : Int }


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { randomSeed } =
    let
        seed =
            initialSeed randomSeed
    in
        { game = Game.init seed } ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    { model | game = Game.update msg model.game } ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions { game } =
    Game.subscriptions game


view : Model -> Html Msg
view ({ game } as model) =
    div []
        [ viewHeader
        , Game.view game
        , viewInfo
        ]


viewHeader : Html Msg
viewHeader =
    div []
        [ h1 []
            [ text "ELM-REX" ]
        , h5 [] [ text "An Elm port of Chrome's T-rex runner game" ]
        ]


viewInfo : Html Msg
viewInfo =
    let
        url =
            "https://github.com/joelchelliah/elm-rex"
    in
        div
            [ class "info" ]
            [ Icon.github (Color.black) 30
            , a [ href url ] [ text "Check it out on github" ]
            ]
