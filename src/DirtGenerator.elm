module DirtGenerator exposing (Model, init, update)

import Dirt
import List exposing (range, map)


type alias Model =
  { spawnX : Float
  , dirtTiles: List Dirt.Model
  }

init : Float -> Model
init xPos =
  let positions = range 0 3 |> map toFloat >> map ((*) Dirt.w)
  in { spawnX = xPos
     , dirtTiles = map Dirt.init positions
     }

update : Float -> Model -> Model
update delta model =
  let pruneDirt = replaceOrUpdate delta model.spawnX
  in { model | dirtTiles = List.map pruneDirt model.dirtTiles }


replaceOrUpdate : Float -> Float -> Dirt.Model -> Dirt.Model
replaceOrUpdate delta xPos dirt =
  if dirt.xPos < -dirt.width
  then Dirt.init xPos
  else Dirt.update delta dirt
