module Cactus exposing (Model, init, update, view)

import MovingElement as Elem
import Svg exposing (Svg)


type alias Model = Elem.Model {}
type alias Msg   = Elem.Msg

init : Float -> Int -> Model
init x i =
  { xPos = x
  , yPos = 38
  , width = w
  , height = h
  , img = "images/cactus_" ++ toString i ++ ".png"
  }

update : Float -> Model -> Model
update = Elem.update

view : (Float, Float) -> Model -> Svg Msg
view = Elem.view


w : Float
w = size.width

h : Float
h = size.height

size : {width: Float, height: Float}
size = { width = 64, height = 84 }
