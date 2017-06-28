module Cactus exposing (Model, init, update, view, bounds)

import WindowSize exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias Model =
    { xPos : Float
    , yPos : Float
    , speed : Float
    , width : Float
    , height : Float
    , img : String
    }


init : Float -> Int -> Model
init x i =
    { xPos = x
    , yPos = 42
    , speed = -0.4
    , width = 64
    , height = 84
    , img = "images/cacti/cactus_" ++ toString i ++ ".png"
    }


type Msg
    = Tick Float


update : Float -> Model -> Model
update delta elem =
    { elem | xPos = elem.xPos + elem.speed * delta }



-- View


view : Model -> Svg Msg
view elem =
    let
        { exMin, eyMin } =
            bounds elem
    in
        Svg.image
            [ x <| toString exMin
            , y <| toString eyMin
            , width <| toString elem.width
            , height <| toString elem.height
            , xlinkHref elem.img
            ]
            []


bounds : Model -> { exMin : Float, exMax : Float, eyMin : Float, eyMax : Float }
bounds elem =
    let
        ( offsetX, offsetY ) =
            ( elem.xPos, windowHeight - offsetFromBottomEdge + elem.yPos )
    in
        { exMin = offsetX
        , exMax = offsetX + elem.width
        , eyMin = offsetY - elem.height
        , eyMax = offsetY
        }


offsetFromBottomEdge : Float
offsetFromBottomEdge =
    100
