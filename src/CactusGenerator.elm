module CactusGenerator exposing (Model, init, update)

import Cactus
import Dict exposing (Dict, fromList, get)
import List exposing (filter, length, map, map2, range)
import Maybe exposing (withDefault)
import Random exposing (Seed, initialSeed, step)
import WindowSize exposing (..)


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


update : Float -> Int -> Model -> Model
update delta score ({ seed, cacti, spawnTimer } as model) =
    let
        ( i, nextSeed ) =
            generateIndex seed

        speedInc =
            toFloat <| score // 500

        updatedCacti =
            map (Cactus.update delta) <| filter Cactus.isVisible cacti
    in
    if spawnTimer == 0 then
        { model
            | cacti = Cactus.init windowWidth i speedInc :: updatedCacti
            , seed = nextSeed
            , spawnTimer = getSpawnTime i <| speedInc
        }

    else
        { model
            | cacti = updatedCacti
            , seed = nextSeed
            , spawnTimer = spawnTimer - 1
        }


generateCactusAt : Float -> Seed -> ( Cactus.Model, Seed )
generateCactusAt position seed0 =
    let
        ( i, seed1 ) =
            generateIndex seed0
    in
    ( Cactus.init position i 0, seed1 )


generateIndex : Seed -> ( Int, Seed )
generateIndex =
    step <| Random.int 0 maxCactusIndex


getSpawnTime : Int -> Float -> Int
getSpawnTime index inc =
    let
        timeDec =
            floor <| inc * 1.8

        withSpawnTime i =
            ( i, 45 + 6 * i - timeDec )

        indices =
            range 0 maxCactusIndex
    in
    withDefault 0 << get index << fromList << map withSpawnTime <| indices


maxCactusIndex : Int
maxCactusIndex =
    5
