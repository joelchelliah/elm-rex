module Hud exposing (Model, Msg(..), init, update, view)

import Svg exposing (Attribute, Svg)
import Svg.Attributes exposing (..)
import WindowSize exposing (..)


type alias Model =
    { score : Int
    , highScore : Int
    , state : State
    }


type State
    = Normal
    | Highlighted


init : Model
init =
    { score = 0
    , highScore = 0
    , state = Normal
    }


type Msg
    = IncScore
    | Reset
    | Highlight


update : Msg -> Model -> Model
update msg ({ score, highScore } as model) =
    case msg of
        IncScore ->
            { model | score = score + 100 }

        Reset ->
            { model
                | score = 0
                , state = Normal
            }

        Highlight ->
            let
                newHighScore =
                    if score > highScore then
                        score

                    else
                        highScore
            in
            { model
                | highScore = newHighScore
                , state = Highlighted
            }


view : Model -> Svg {}
view hud =
    Svg.svg []
        [ renderOutline hud
        , renderScore hud
        , renderHighScore hud
        ]


renderOutline : Model -> Svg {}
renderOutline { state } =
    let
        ( col, alpha ) =
            highlightStyles state
    in
    Svg.rect
        [ style <| "fill:none;stroke:" ++ col ++ ";stroke-width:5;opacity:" ++ alpha
        , x "0"
        , y "0"
        , width (String.fromFloat <| windowWidth)
        , height (String.fromFloat <| windowHeight)
        ]
        []


renderScore : Model -> Svg {}
renderScore { score, state } =
    let
        ( col, alpha ) =
            highlightStyles state

        attrs =
            [ x "20"
            , y "40"
            , fill col
            , opacity alpha
            , fontSize "25"
            , textAnchor "start"
            ]
    in
    Svg.text_ attrs [ Svg.text <| "Score: " ++ String.fromInt score ]


renderHighScore : Model -> Svg {}
renderHighScore { highScore, state } =
    let
        ( col, alpha ) =
            highlightStyles state

        attrs =
            [ x "980"
            , y "40"
            , fill col
            , opacity alpha
            , fontSize "25"
            , textAnchor "end"
            ]
    in
    Svg.text_ attrs [ Svg.text <| "High Score: " ++ String.fromInt highScore ]


highlightStyles : State -> ( String, String )
highlightStyles state =
    if state == Highlighted then
        ( "red", "0.85" )

    else
        ( "black", "0.6" )
