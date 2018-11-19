module Cactus exposing (Model, bounds, init, isVisible, update, view)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import WindowSize exposing (..)


type alias Model =
    { xPos : Float
    , yPos : Float
    , speed : Float
    , width : Float
    , height : Float
    , img : String
    }


init : Float -> Int -> Float -> Model
init x i speedInc =
    { xPos = x
    , yPos = 42
    , speed = 0.4 + 0.02 * speedInc
    , width = 64
    , height = 84
    , img = "images/cacti/cactus_" ++ String.fromInt i ++ ".png"
    }


update : Float -> Model -> Model
update delta cactus =
    { cactus | xPos = cactus.xPos - cactus.speed * delta }


view : Model -> Svg {}
view cactus =
    let
        { exMin, eyMin } =
            bounds cactus
    in
    Svg.image
        [ x <| String.fromFloat exMin
        , y <| String.fromFloat eyMin
        , width <| String.fromFloat cactus.width
        , height <| String.fromFloat cactus.height
        , xlinkHref cactus.img
        ]
        []


isVisible : Model -> Bool
isVisible cactus =
    cactus.xPos >= -cactus.width


bounds : Model -> { exMin : Float, exMax : Float, eyMin : Float, eyMax : Float }
bounds cactus =
    let
        offsetFromBottomEdge =
            100

        ( offsetX, offsetY ) =
            ( cactus.xPos, windowHeight - offsetFromBottomEdge + cactus.yPos )
    in
    { exMin = offsetX
    , exMax = offsetX + cactus.width
    , eyMin = offsetY - cactus.height
    , eyMax = offsetY
    }
