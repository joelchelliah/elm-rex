module CactusGenerator exposing (Model, init, update)

import Cactus
import Random exposing (Seed, initialSeed, step)
import List exposing (length, map2)
import Dict exposing (Dict, fromList, get)


type alias Model =
    { spawnX : Float
    , seed : Seed
    , cacti : List Cactus.Model
    }


init : Float -> Seed -> Model
init xPos seed0 =
    let
        cactiPositions =
            [ xPos - 200, xPos + 200, xPos + 500 ]

        ( cacti, seed1 ) =
            generateCacti cactiPositions seed0
    in
        { spawnX = xPos
        , seed = seed1
        , cacti = cacti
        }


update : Float -> Model -> Model
update delta model =
    let
        ( i, nextSeed ) =
            generateIndex model.seed

        pruneCactus =
            replaceOrUpdate delta model.spawnX i
    in
        { model
            | cacti = List.map pruneCactus model.cacti
            , seed = nextSeed
        }


replaceOrUpdate : Float -> Float -> Int -> Cactus.Model -> Cactus.Model
replaceOrUpdate delta xPos index cactus =
    if cactus.xPos < -cactus.width then
        Cactus.init (xPos + cactusPositionOffset index) index
    else
        Cactus.update delta cactus


generateCacti : List Float -> Seed -> ( List Cactus.Model, Seed )
generateCacti positions seed0 =
    let
        numCacti =
            length positions

        ( is, seed1 ) =
            generateIndices numCacti seed0

        gen ( pos, i ) =
            Cactus.init pos i

        randomCacti =
            List.map gen <| map2 (,) positions is
    in
        ( randomCacti, seed1 )


generateIndices : Int -> Seed -> ( List Int, Seed )
generateIndices num seed =
    let
        randomIndex =
            Random.int 0 maxCactusIndex

        randomIndices =
            Random.list num randomIndex
    in
        step randomIndices seed


generateIndex : Seed -> ( Int, Seed )
generateIndex seed0 =
    let
        ( indices, seed1 ) =
            generateIndices 1 seed0

        takeFirstFrom =
            List.head >> Maybe.withDefault 0
    in
        ( takeFirstFrom indices, seed1 )


cactusPositionOffset : Int -> Float
cactusPositionOffset i =
    let
        offsets =
            fromList
                [ ( 0, 40 )
                , ( 1, 80 )
                , ( 2, 120 )
                , ( 3, 160 )
                , ( 4, 200 )
                ]
    in
        Maybe.withDefault 0 <| get i offsets


maxCactusIndex : Int
maxCactusIndex =
    4
