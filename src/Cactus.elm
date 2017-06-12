module Cactus exposing (Model, init, update)

import MovingElement as Elem


type alias Model =
    Elem.Model


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
