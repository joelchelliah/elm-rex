module CactusGenerator exposing (Model, init, update)

import Cactus
import Random exposing (Seed, initialSeed, step)


type alias Model =
  { spawnX : Float
  , seed : Seed
  , cacti: List Cactus.Model
  }

init : Float -> Seed -> Model
init xPos seed0 =
  let numberOfCacti    = 3
      (indices, seed1) = generateIndices numberOfCacti seed0
      generateCactus i = Cactus.init ((xPos + (toFloat i) * xPos) / 2) i
  in { spawnX = xPos
     , seed   = seed1
     , cacti  = List.map generateCactus indices
    }

update : Float -> Model -> Model
update delta model =
  let (i, nextSeed) = generateIndex model.seed
      pruneCactus   = replaceOrUpdate delta model.spawnX i
  in { model | cacti = List.map pruneCactus model.cacti
             , seed  = nextSeed }


replaceOrUpdate : Float -> Float -> Int -> Cactus.Model -> Cactus.Model
replaceOrUpdate delta xPos index cactus =
  if cactus.xPos < -cactus.width
  then Cactus.init xPos index
  else Cactus.update delta cactus

generateIndices : Int -> Seed -> (List Int, Seed)
generateIndices num seed =
  let randomIndex   = Random.int 0 2
      randomIndices = Random.list num randomIndex
  in step randomIndices seed

generateIndex : Seed -> (Int, Seed)
generateIndex seed0 =
  let (indices, seed1) = generateIndices 1 seed0
      takeFirstFrom    = List.head >> Maybe.withDefault 0
  in (takeFirstFrom indices, seed1)
