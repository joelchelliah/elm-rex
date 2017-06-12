module Cactus exposing (Model, init, update, view)

import MovingElement as Elem
import Svg exposing (Svg)


type alias Model =
    Elem.Model


type alias Msg =
    Elem.Msg


init : Float -> Int -> Model
init x i =
    { xPos = x
    , yPos = 42
    , speed = -0.4
    , width = 64
    , height = 84
    , img = "images/cacti/cactus_" ++ toString i ++ ".png"
    }


update : Float -> Model -> Model
update =
    Elem.update


view : Model -> Svg Msg
view =
    Elem.view
