module Cloud exposing (Model, init, update)

import MovingElement as Elem


type alias Model =
    Elem.Model


init : Float -> Float -> Int -> Model
init x spd i =
    { xPos = x
    , yPos = -200
    , speed = spd
    , width = 148
    , height = 60
    , img = "images/clouds/cloud_" ++ toString i ++ ".png"
    }


update : Float -> Model -> Model
update =
    Elem.update
