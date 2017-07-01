module Game exposing (..)

import Hud
import Rex
import Cactus
import CactusGenerator as CactusGen
import Cloud
import Background
import WindowSize exposing (..)
import Html exposing (Html, programWithFlags, h1, h5, div, map, a, text)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import AnimationFrame
import Random exposing (Seed, step)


type GameState
    = New
    | Playing
    | Paused
    | GameOver


type alias Model =
    { state : GameState
    , hud : Hud.Model
    , rex : Rex.Model
    , cactusGen : CactusGen.Model
    , cloud : Cloud.Model
    , seed : Seed
    }


init : Seed -> Model
init seed =
    { state = New
    , hud = Hud.init
    , rex = Rex.init
    , cactusGen = CactusGen.init seed
    , cloud = Cloud.init
    , seed = nextSeed seed
    }


restart : Model -> Model
restart game =
    { state = New
    , hud = Hud.update Hud.Reset game.hud
    , rex = Rex.init
    , cactusGen = CactusGen.init game.seed
    , cloud = game.cloud
    , seed = nextSeed game.seed
    }


type Msg
    = Tick Time
    | KeyPressed KeyCode
    | KeyReleased
    | SubMsg


update : Msg -> Model -> Model
update msg game =
    case game.state of
        Playing ->
            updatePlaying msg game

        GameOver ->
            updateGameOver msg game

        _ ->
            updatePaused msg game


updatePlaying : Msg -> Model -> Model
updatePlaying msg ({ hud, rex, cactusGen, cloud } as game) =
    if (spacePressed msg) then
        { game | state = Paused }
    else
        case msg of
            KeyPressed code ->
                { game | rex = Rex.update (codeToRexMsg code) rex }

            KeyReleased ->
                { game | rex = Rex.update Rex.Run rex }

            Tick delta ->
                if Rex.hitDetected rex cactusGen.cacti then
                    { game
                        | state = GameOver
                        , hud = Hud.update Hud.Highlight hud
                        , rex = Rex.update Rex.Kill rex
                        , cloud = Cloud.update delta cloud
                    }
                else
                    { game
                        | rex = Rex.update (Rex.Tick delta) rex
                        , cactusGen = CactusGen.update delta hud.score cactusGen
                        , cloud = Cloud.update delta cloud
                        , hud =
                            if (Rex.hasLandedFromJumping rex) then
                                Hud.update Hud.IncScore hud
                            else
                                hud
                    }

            SubMsg ->
                game


updatePaused : Msg -> Model -> Model
updatePaused msg game =
    if (spacePressed msg) then
        { game | state = Playing }
    else
        game


updateGameOver : Msg -> Model -> Model
updateGameOver msg game =
    if (spacePressed msg) then
        restart game
    else
        case msg of
            Tick delta ->
                { game
                    | rex = Rex.update (Rex.Tick delta) game.rex
                    , cloud = Cloud.update delta game.cloud
                }

            _ ->
                game


codeToRexMsg : KeyCode -> Rex.Msg
codeToRexMsg code =
    case code of
        40 ->
            Rex.Duck

        38 ->
            Rex.Jump

        _ ->
            Rex.Run


spacePressed : Msg -> Bool
spacePressed msg =
    msg == KeyPressed 32


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyPressed
        , Keyboard.ups (\_ -> KeyReleased)
        ]


view : Model -> Html Msg
view { state, hud, rex, cactusGen, cloud } =
    let
        ( w, h ) =
            ( toString windowWidth, toString windowHeight )

        attributes =
            [ width w
            , height h
            , viewBox <| "0 0 " ++ w ++ " " ++ h
            , version "1.1"
            ]

        sceneElements =
            [ viewBackground
            , viewCacti cactusGen.cacti
            , viewCloud cloud
            , viewRex rex
            , viewAlert state hud.score
            , viewHud hud
            ]
    in
        Svg.svg attributes sceneElements


viewAlert : GameState -> Int -> Svg Msg
viewAlert state score =
    let
        yMiddle =
            windowHeight / 2

        attrBase =
            [ x << toString <| windowWidth / 2
            , textAnchor "middle"
            , fill "#C12"
            ]

        attrLarge yPos =
            [ y << toString <| yMiddle + yPos
            , fontSize "60"
            ]
                ++ attrBase

        attrSmall yPos =
            [ y << toString <| yMiddle + yPos
            , fontSize "18"
            ]
                ++ attrBase
    in
        case state of
            New ->
                Svg.svg []
                    [ Svg.text_ (attrLarge -50) [ Svg.text "RAWЯ!" ]
                    , Svg.text_ (attrSmall 0) [ Svg.text "Play using the arrow keys: ↑ ↓" ]
                    , Svg.text_ (attrSmall 35) [ Svg.text "Press SPACE to start / pause" ]
                    ]

            Paused ->
                Svg.svg []
                    [ Svg.text_ (attrLarge -50) [ Svg.text "Paused!" ]
                    , Svg.text_ (attrSmall 0) [ Svg.text "Play using the arrow keys: ↑ ↓" ]
                    , Svg.text_ (attrSmall 35) [ Svg.text "Press SPACE to continue" ]
                    ]

            GameOver ->
                Svg.svg []
                    [ Svg.text_ (attrLarge -40) [ Svg.text "Game Ovər!" ]
                    , Svg.text_ (attrSmall 25) [ Svg.text "Press SPACE to try again" ]
                    ]

            Playing ->
                Svg.svg [] []


viewBackground : Svg Msg
viewBackground =
    map (\_ -> SubMsg) Background.view


viewCacti : List Cactus.Model -> Svg Msg
viewCacti elems =
    let
        render elem =
            map (\_ -> SubMsg) (Cactus.view elem)
    in
        Svg.svg [] <| List.map render elems


viewCloud : Cloud.Model -> Svg Msg
viewCloud cloud =
    map (\_ -> SubMsg) (Cloud.view cloud)


viewRex : Rex.Model -> Svg Msg
viewRex rex =
    map (\_ -> SubMsg) (Rex.view rex)


viewHud : Hud.Model -> Svg Msg
viewHud hud =
    map (\_ -> SubMsg) (Hud.view hud)


nextSeed : Seed -> Seed
nextSeed seed =
    Tuple.second <| step Random.bool seed
