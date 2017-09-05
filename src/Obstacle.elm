module Obstacle exposing (Model, Kind(..), init, update, view, isVisible, bounds)

import WindowSize exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)



type Kind
    = Cactus
    | Ufo
    | UfoBig


type alias Model =
    { xPos : Float
    , yPos : Float
    , speed : Float
    , width : Float
    , height : Float
    , img : String
    , kind : Kind
    }


init : Float -> Int -> Float -> Kind -> Model
init x i speedInc kind =
    let 
        (img, y, width, height) = case kind of
            Ufo -> ("images/ufo/ufo.png", -20, 64, 38)
            UfoBig -> ("images/ufo/ufo_big.png", -20, 135, 80)
            _ -> ("images/cacti/cactus_" ++ toString i ++ ".png", 42, 64, 84)            
    in
        { xPos = x
        , yPos = y
        , speed = 0.4 + 0.02 * speedInc
        , width = width
        , height = height
        , kind = kind
        , img = img
        }
 

update : Float -> Model -> Model
update delta obstacle =
    { obstacle | xPos = obstacle.xPos - obstacle.speed * delta }


view : Model -> Svg {}
view obstacle =
    let
        { exMin, eyMin } =
            bounds obstacle
    in
        Svg.image
            [ x <| toString exMin
            , y <| toString eyMin
            , width <| toString obstacle.width
            , height <| toString obstacle.height
            , xlinkHref obstacle.img
            ]
            []


isVisible : Model -> Bool
isVisible obstacle =
    obstacle.xPos >= -obstacle.width


bounds : Model -> { exMin : Float, exMax : Float, eyMin : Float, eyMax : Float }
bounds obstacle =
    let
        offsetFromBottomEdge =
            100

        ( offsetX, offsetY ) =
            ( obstacle.xPos, windowHeight - offsetFromBottomEdge + obstacle.yPos )
    in
        { exMin = offsetX
        , exMax = offsetX + obstacle.width
        , eyMin = offsetY - obstacle.height
        , eyMax = offsetY
        }
