module CloudGenerator exposing (Model, init, update)

import Cloud
import WindowSize exposing (..)
import Random exposing (Seed, initialSeed, step)
import List exposing (length, map, map3)


type alias Model =
    { seed : Seed
    , clouds : List Cloud.Model
    }


init : Seed -> Model
init seed0 =
    let
        positions =
            [ 200, 500 ]

        ( clouds, seed1 ) =
            generate positions seed0
    in
        { seed = seed1
        , clouds = clouds
        }


update : Float -> Model -> Model
update delta model =
    let
        ( index, seed1 ) =
            generateIndex model.seed

        ( speed, seed2 ) =
            generateSpeed seed1

        prune =
            replaceOrUpdate delta speed index
    in
        { model
            | seed = seed2
            , clouds = List.map prune model.clouds
        }


replaceOrUpdate : Float -> Float -> Int -> Cloud.Model -> Cloud.Model
replaceOrUpdate delta newSpeed index cloud =
    let
        cloudGoneLeft =
            cloud.speed < 0 && cloud.xPos < -cloud.width

        cloudGoneRight =
            cloud.speed > 0 && cloud.xPos > windowWidth

        newX =
            if newSpeed < 0 then
                windowWidth
            else
                -cloud.width
    in
        if cloudGoneLeft || cloudGoneRight then
            Cloud.init newX newSpeed index
        else
            Cloud.update delta cloud


generate : List Float -> Seed -> ( List Cloud.Model, Seed )
generate positions seed0 =
    let
        num =
            length positions

        ( inidices, seed1 ) =
            generateIndices num seed0

        ( speeds, seed2 ) =
            generateSpeeds num seed1

        gen ( pos, speed, i ) =
            Cloud.init pos speed i

        randomCacti =
            List.map gen <| map3 (,,) positions speeds inidices
    in
        ( randomCacti, seed1 )


generateIndices : Int -> Seed -> ( List Int, Seed )
generateIndices num seed =
    let
        randomIndex =
            Random.int 0 2

        randomIndices =
            Random.list num randomIndex
    in
        step randomIndices seed


generateSpeeds : Int -> Seed -> ( List Float, Seed )
generateSpeeds num seed0 =
    let
        speedBaseGen =
            Random.list num <| Random.int -2 1

        ( speedBases, seed1 ) =
            step speedBaseGen seed0

        toSpeed i =
            if i == 0 then
                0.02
            else
                (toFloat i) / 100

        speeds =
            map toSpeed speedBases
    in
        ( speeds, seed1 )


generateIndex : Seed -> ( Int, Seed )
generateIndex seed0 =
    let
        ( indices, seed1 ) =
            generateIndices 1 seed0

        takeFirstFrom =
            List.head >> Maybe.withDefault 0
    in
        ( takeFirstFrom indices, seed1 )


generateSpeed : Seed -> ( Float, Seed )
generateSpeed seed0 =
    let
        ( speeds, seed1 ) =
            generateSpeeds 1 seed0

        takeFirstFrom =
            List.head >> Maybe.withDefault 0.02
    in
        ( takeFirstFrom speeds, seed1 )
