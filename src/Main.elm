module Main exposing (..)

import Hud
import Rex
import CactusGenerator as CactusGen
import MovingElement as Elem
import Background
import WindowSize exposing (..)
import Html exposing (Html, programWithFlags, div, map)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import AnimationFrame
import Random exposing (initialSeed)


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


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
    , seed : Int
    }


type alias Flags =
    { randomSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init { randomSeed } =
    let
        model =
            { state = New
            , hud = Hud.init
            , rex = Rex.init
            , cactusGen = CactusGen.init windowWidth <| initialSeed randomSeed
            , seed = randomSeed
            }
    in
        ( model, Cmd.none )



-- Update


type Msg
    = Tick Time
    | KeyPressed KeyCode
    | KeyReleased
    | SubMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Playing ->
            ( updatePlaying msg model, Cmd.none )

        GameOver ->
            ( updateGameOver msg model, Cmd.none )

        _ ->
            ( updatePaused msg model, Cmd.none )


updatePlaying : Msg -> Model -> Model
updatePlaying msg ({ hud, rex, cactusGen } as model) =
    if (spacePressed msg) then
        { model | state = Paused }
    else
        case msg of
            KeyPressed code ->
                { model | rex = Rex.update (codeToRexMsg code) rex }

            KeyReleased ->
                { model | rex = Rex.update Rex.Run rex }

            Tick delta ->
                if Rex.hitDetected rex cactusGen.cacti then
                    { model
                        | state = GameOver
                        , hud = Hud.update Hud.Highlight hud
                        , rex = Rex.update Rex.Kill rex
                    }
                else
                    { model
                        | rex = Rex.update (Rex.Tick delta) rex
                        , cactusGen = CactusGen.update delta cactusGen
                        , hud =
                            if (Rex.hasLanded rex) then
                                Hud.update Hud.IncScore hud
                            else
                                hud
                    }

            SubMsg ->
                model


updatePaused : Msg -> Model -> Model
updatePaused msg model =
    if (spacePressed msg) then
        { model | state = Playing }
    else
        model


updateGameOver : Msg -> Model -> Model
updateGameOver msg model =
    if (spacePressed msg) then
        Tuple.first <| init { randomSeed = model.seed }
    else
        model


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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyPressed
        , Keyboard.ups (\_ -> KeyReleased)
        ]



-- View


view : Model -> Html Msg
view ({ state, hud, rex, cactusGen } as model) =
    let
        ( w, h ) =
            ( toString windowWidth, toString windowHeight )

        attributes =
            [ width w
            , height h
            , viewBox <| "0 0 " ++ w ++ " " ++ h
            , version "1.1"
            , style "position: fixed;"
            ]

        sceneElements =
            [ renderBackground
            , renderMovingElements cactusGen.cacti
            , renderRex rex
            , renderMessages state hud.score
            , renderHud hud
            ]
    in
        Svg.svg attributes sceneElements


renderMessages : GameState -> Int -> Svg Msg
renderMessages state score =
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
                    , Svg.text_ (attrSmall 35) [ Svg.text "Press SPACE to pause" ]
                    ]

            Paused ->
                Svg.svg []
                    [ Svg.text_ (attrLarge -50) [ Svg.text "Paused!" ]
                    , Svg.text_ (attrSmall 0) [ Svg.text "Play using the arrow keys: ↑ ↓" ]
                    , Svg.text_ (attrSmall 35) [ Svg.text "Press SPACE to continue" ]
                    ]

            GameOver ->
                Svg.svg []
                    [ Svg.text_ (attrLarge -50) [ Svg.text "Game Ovər!" ]
                    , Svg.text_ (attrSmall 35) [ Svg.text "Press SPACE to try again" ]
                    ]

            Playing ->
                Svg.svg [] []


renderBackground : Svg Msg
renderBackground =
    map (\_ -> SubMsg) Background.view


renderMovingElements : List (Elem.Model a) -> Svg Msg
renderMovingElements elems =
    let
        render elem =
            map (\_ -> SubMsg) (Elem.view elem)
    in
        Svg.svg [] <| List.map render elems


renderRex : Rex.Model -> Svg Msg
renderRex rex =
    map (\_ -> SubMsg) (Rex.view rex)


renderHud : Hud.Model -> Svg Msg
renderHud hud =
    map (\_ -> SubMsg) (Hud.view hud)
