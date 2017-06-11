module Hud exposing (Model, Msg(..), init, update, view)

import WindowSize exposing (..)
import Svg exposing (Svg, Attribute)
import Svg.Attributes exposing (..)


-- Model


type alias Model =
    { score : Int
    , state : State
    }


type State
    = Normal
    | Highlighted


init : Model
init =
    { score = 0
    , state = Normal
    }



-- Update


type Msg
    = IncScore
    | Highlight


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncScore ->
            { model | score = model.score + 100 }

        Highlight ->
            { model | state = Highlighted }



-- View


view : Model -> Svg {}
view hud =
    Svg.svg []
        [ renderOutline hud
        , renderScore hud
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
            , width (toString <| windowWidth)
            , height (toString <| windowHeight)
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
            ]
    in
        Svg.text_ attrs [ Svg.text <| "Score: " ++ (toString score) ]


highlightStyles : State -> ( String, String )
highlightStyles state =
    if state == Highlighted then
        ( "red", "0.75" )
    else
        ( "black", "0.4" )
