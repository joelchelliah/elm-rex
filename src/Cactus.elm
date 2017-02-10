module Cactus exposing (Model, init, update, view, move)

import MovingElement as Elem
import Svg exposing (Svg)
import Random exposing (Seed, initialSeed, step)


type alias Model = Elem.Model { seed: Seed }
type alias Msg   = Elem.Msg

init : Float -> Seed -> Model
init x seed0 =
  let randomInt  = Random.int 0 2
      (i, seed1) = step randomInt seed0
  in { xPos = x
     , yPos = 50
     , width = w
     , height = h
     , img = "images/cactus_" ++ toString i ++ ".png"
     , seed = seed1
     }

update : Float -> Model -> Model
update = Elem.update

view : (Float, Float) -> Model -> Svg Msg
view = Elem.view

move : Float -> Float -> List Model -> List Model
move x delta cacti =
  let prune = replaceOrUpdate x delta
  in List.map prune cacti

w : Float
w = size.width

h : Float
h = size.height


size : {width: Float, height: Float}
size = { width = 64, height = 84 }

replaceOrUpdate : Float -> Float -> Model -> Model
replaceOrUpdate x delta cactus =
  if cactus.xPos < -cactus.width
  then init x cactus.seed
  else update delta cactus
