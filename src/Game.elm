module Game exposing (GameState(..), Model, Msg(..), init, nextSeed, restart, subscriptions, update, updateGameOver, updatePaused, updatePlaying, view, viewAlert, viewBackground, viewCacti, viewCloud, viewHud, viewRex)

import Background
import Browser.Events as Events
import Cactus
import CactusGenerator as CactusGen
import Cloud
import Controls
import Html exposing (Html, a, div, h1, h5, map, text)
import Hud
import Json.Decode as Decode
import Random exposing (Seed, step)
import Rex
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import WindowSize exposing (..)


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
    = Tick Float
    | KeyPressed Controls.Key
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
    case msg of
        KeyPressed Controls.Space ->
            { game | state = Paused }

        KeyPressed key ->
            { game | rex = Rex.update (keyToRexMsg key) rex }

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
                        if Rex.hasLandedFromJumping rex then
                            Hud.update Hud.IncScore hud

                        else
                            hud
                }

        SubMsg ->
            game


updatePaused : Msg -> Model -> Model
updatePaused msg game =
    case msg of
        KeyPressed Controls.Space ->
            { game | state = Playing }

        _ ->
            game


updateGameOver : Msg -> Model -> Model
updateGameOver msg game =
    case msg of
        KeyPressed Controls.Space ->
            restart game

        Tick delta ->
            { game
                | rex = Rex.update (Rex.Tick delta) game.rex
                , cloud = Cloud.update delta game.cloud
            }

        _ ->
            game


keyToRexMsg : Controls.Key -> Rex.Msg
keyToRexMsg code =
    case code of
        Controls.Up ->
            Rex.Jump

        Controls.Down ->
            Rex.Duck

        Controls.Space ->
            Rex.Run


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta Tick
        , Events.onKeyDown (Controls.decoder |> Decode.map KeyPressed)
        , Events.onKeyUp (Decode.succeed KeyReleased)
        ]


view : Model -> Html Msg
view { state, hud, rex, cactusGen, cloud } =
    let
        ( w, h ) =
            ( String.fromFloat windowWidth, String.fromFloat windowHeight )

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
            [ x << String.fromFloat <| windowWidth / 2
            , textAnchor "middle"
            , fill "#C12"
            ]

        attrLarge yPos =
            [ y << String.fromFloat <| yMiddle + yPos
            , fontSize "60"
            ]
                ++ attrBase

        attrSmall yPos =
            [ y << String.fromFloat <| yMiddle + yPos
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
    Tuple.second <| step (Random.constant True) seed
