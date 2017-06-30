module Cactus exposing (Model, init, update, view, isVisible, bounds)

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


init : Float -> Int -> Float -> Model
init x i speedInc =
    { xPos = x
    , yPos = 42
    , speed = 0.4 + 0.05 * speedInc
    , width = 64
    , height = 84
    , img = "images/cacti/cactus_" ++ toString i ++ ".png"
    }


update : Float -> Model -> Model
update delta cactus =
    { cactus | xPos = cactus.xPos - cactus.speed * delta }



-- View


view : Model -> Svg {}
view cactus =
    let
        { exMin, eyMin } =
            bounds cactus
    in
        Svg.image
            [ x <| toString exMin
            , y <| toString eyMin
            , width <| toString cactus.width
            , height <| toString cactus.height
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
