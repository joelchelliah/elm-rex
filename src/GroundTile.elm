module GroundTile exposing (Model, init, update, view, move, w, h)

import MovingElement as Elem
import Svg exposing (Svg)


type alias Model = Elem.Model
type alias Msg   = Elem.Msg

init : Float -> Model
init x = Elem.init x 0 w h "images/ground.png"

update : Float -> Model -> Model
update = Elem.update

view : (Float, Float) -> Model -> Svg Msg
view = Elem.view

move : Float -> Float -> List Model -> List Model
move x deltaX tiles =
  let prune = replaceOrUpdate x deltaX
  in List.map prune tiles

w : Float
w = size.width

h : Float
h = size.height


size : {width: Float, height: Float}
size = { width = 400, height = 100 }

replaceOrUpdate : Float -> Float -> Model -> Model
replaceOrUpdate x deltaX tile =
  let errorMargin = -10
  in if tile.xPos < -tile.width
     then init (x + errorMargin)
     else update deltaX tile
