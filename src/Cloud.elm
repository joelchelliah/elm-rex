module Cloud exposing (Model, init, update, view)

import MovingElement as Elem
import Svg exposing (Svg)


type alias Model =
    Elem.Model


type alias Msg =
    Elem.Msg


init : Float -> Float -> Int -> Model
init x spd i =
    { xPos = x
    , yPos = -100
    , speed = spd
    , width = 148
    , height = 60
    , img = "images/clouds/cloud_" ++ toString i ++ ".png"
    }


update : Float -> Model -> Model
update =
    Elem.update


view : Model -> Svg Msg
view =
    Elem.view
