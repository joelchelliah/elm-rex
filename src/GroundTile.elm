module GroundTile exposing (Model, init, update, view, w, h)

import MovingElement as Elem

import Svg exposing (Svg, Attribute)


type alias Model = Elem.Model
type alias Msg   = Elem.Msg

init : Float -> Model
init x = Elem.init x 0 w h "images/ground.png"


update : Float -> Model -> Model
update = Elem.update


view : (Float, Float) -> Model -> Svg Msg
view = Elem.view

w : Float
w = size.width

h : Float
h = size.height

size : {width: Float, height: Float}
size = { width = 400, height = 100 }
