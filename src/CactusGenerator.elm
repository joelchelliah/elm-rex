module CactusGenerator exposing (Model, init, update)

import Cactus
import WindowSize exposing (..)
import Random exposing (Seed, initialSeed, step)
import List exposing (length, range, map, map2, filter)
import Dict exposing (Dict, fromList, get)
import Maybe exposing (withDefault)


type alias Model =
    { seed : Seed
    , cacti : List Cactus.Model
    , spawnTimer : Int
    }


init : Seed -> Model
init seed0 =
    let
        startPosition =
            windowWidth - 200

        ( cactus, seed1 ) =
            generateCactusAt startPosition seed0
    in
        { seed = seed1
        , cacti = [ cactus ]
        , spawnTimer = 50
        }


update : Float -> Model -> Model
update delta model =
    let
        ( i, nextSeed ) =
            generateIndex model.seed

        updatedCacti =
            map (Cactus.update delta) <| filter Cactus.isVisible model.cacti
    in
        if model.spawnTimer == 0 then
            { model
                | cacti = (Cactus.init windowWidth i) :: updatedCacti
                , seed = nextSeed
                , spawnTimer = getSpawnTime i
            }
        else
            { model
                | cacti = updatedCacti
                , seed = nextSeed
                , spawnTimer = model.spawnTimer - 1
            }


generateCactusAt : Float -> Seed -> ( Cactus.Model, Seed )
generateCactusAt position seed0 =
    let
        ( i, seed1 ) =
            generateIndex seed0
    in
        ( Cactus.init position i, seed1 )


generateIndex : Seed -> ( Int, Seed )
generateIndex =
    step <| Random.int 0 maxCactusIndex


getSpawnTime : Int -> Int
getSpawnTime index =
    let
        indices =
            range 0 maxCactusIndex

        withSpawnTime i =
            ( i, 40 + 7 * i )
    in
        withDefault 0 << get index << fromList << map withSpawnTime <| indices


maxCactusIndex : Int
maxCactusIndex =
    4
