module Cactus exposing (Model, init, update, view, move)

import MovingElement as Elem
import Svg exposing (Svg)


type alias Model = Elem.Model
type alias Msg   = Elem.Msg

init : Float -> Model
init x = Elem.init x 50 w h "images/cactus_1.png"

update : Float -> Model -> Model
update = Elem.update

view : (Float, Float) -> Model -> Svg Msg
view = Elem.view

move : Float -> Float -> List Model -> List Model
move x deltaX cacti =
  let prune = replaceOrUpdate x deltaX
  in List.map prune cacti

w : Float
w = size.width

h : Float
h = size.height


size : {width: Float, height: Float}
size = { width = 100, height = 100 }

replaceOrUpdate : Float -> Float -> Model -> Model
replaceOrUpdate x deltaX cactus =
  if cactus.xPos < -cactus.width
  then init x
  else update deltaX cactus
