module Cactus exposing (Model, init, update, view)

import MovingElement as Elem

import Svg exposing (Svg, Attribute)


type alias Model = Elem.Model
type alias Msg   = Elem.Msg

init : Float -> Model
init x =
  let (w, h) = (100, 100)
      y      = 0
      img    = "images/cactus_1.png"
  in Elem.init x y w h img


update : Float -> Model -> Model
update = Elem.update


view : (Int,Int) -> Model -> Svg Msg
view = Elem.view
