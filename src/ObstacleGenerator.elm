module ObstacleGenerator exposing (Model, init, update)

import Obstacle
import WindowSize exposing (..)
import Random exposing (Seed, initialSeed, step, int)
import List exposing (length, range, map, map2, filter)
import Dict exposing (Dict, fromList, get)
import Maybe exposing (withDefault)


type alias Model =
    { seed : Seed
    , obstacles : List Obstacle.Model
    , spawnTimer : Int
    }


init : Seed -> Model
init seed0 =
    let
        startPosition =
            windowWidth - 200
        
        ( obstacle, seed1 ) =
            generateObstacleAt startPosition seed0
    in
        { seed = seed1
        , obstacles = [ obstacle ]
        , spawnTimer = 50
        }


update : Float -> Int -> Model -> Model
update delta score ({ seed, obstacles, spawnTimer } as model) =
    let
        ( i, nextSeed ) =
            generateIndex seed

        (ufoFactor, _ ) = step (int 0 5) seed

        kind = if ufoFactor == 0 then Obstacle.UfoBig 
            else if ufoFactor <= 2 then Obstacle.Ufo
            else Obstacle.Cactus

        speedInc =
            toFloat <| score // 500

        updatedObstacles =
            map (Obstacle.update delta) <| filter Obstacle.isVisible obstacles
    in
        if spawnTimer == 0 then
            { model
                | obstacles = (Obstacle.init windowWidth i speedInc kind) :: updatedObstacles
                , seed = nextSeed
                , spawnTimer = getSpawnTime i <| speedInc
            }
        else
            { model
                | obstacles = updatedObstacles
                , seed = nextSeed
                , spawnTimer = spawnTimer - 1
            }


generateObstacleAt : Float -> Seed -> ( Obstacle.Model, Seed )
generateObstacleAt x seed0 =
    let
        ( i, seed1 ) =
            generateIndex seed0
    in
        ( Obstacle.init x i 0 Obstacle.Cactus, seed1 )


generateIndex : Seed -> ( Int, Seed )
generateIndex =
    step <| Random.int 0 maxObstacleIndex


getSpawnTime : Int -> Float -> Int
getSpawnTime index inc =
    let
        timeDec =
            floor <| inc * 1.8

        withSpawnTime i =
            ( i, 45 + 6 * i - timeDec )

        indices =
            range 0 maxObstacleIndex
    in
        withDefault 0 << get index << fromList << map withSpawnTime <| indices


maxObstacleIndex : Int
maxObstacleIndex =
    5
