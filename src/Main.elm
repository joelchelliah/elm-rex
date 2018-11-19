module Main exposing (Flags, Model, init, main, subscriptions, update, view, viewHeader, viewInfo)

import Browser
import FeatherIcons as Icon
import Game exposing (Msg)
import Html exposing (Html, a, div, h1, h5, img, map, text)
import Html.Attributes exposing (href, src)
import Random exposing (Seed, initialSeed)
import Svg.Attributes exposing (..)


type alias Model =
    { game : Game.Model
    }


type alias Flags =
    { randomSeed : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
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
    ( { game = Game.init seed }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model | game = Game.update msg model.game }
    , Cmd.none
    )


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
            [ img [ src "images/logo_right.png" ] []
            , text "ELM-REX"
            , img [ src "images/logo_left.png" ] []
            ]
        , h5 [] [ text "- An Elm port of Chrome's T-rex runner game -" ]
        ]


viewInfo : Html Msg
viewInfo =
    let
        url =
            "https://github.com/joelchelliah/elm-rex"
    in
    div
        [ class "info" ]
        [ Icon.github |> Icon.toHtml []
        , a [ href url ] [ text "Find it on github" ]
        ]
