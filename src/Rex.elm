module Rex exposing (Model, Msg(..), init, update, hasLandedFromJumping, hitDetected, view)

import Cactus
import WindowSize exposing (..)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (..)
import Time exposing (Time)


type alias Model =
    { state : State
    , yPos : Float
    , yVel : Float
    , width : Float
    , height : Float
    , runCount : Int
    , frameInc : Int
    }


type State
    = Idle
    | Running
    | Jumping
    | Ducking
    | Dead


init : Model
init =
    { state = Idle
    , yPos = 0
    , yVel = 0
    , width = sizeRunning.width
    , height = sizeRunning.height
    , runCount = 1
    , frameInc = 0
    }


type Msg
    = Run
    | Jump
    | Duck
    | Kill
    | Tick Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Run ->
            case model.state of
                Jumping ->
                    model

                _ ->
                    animate Running model

        Jump ->
            case model.state of
                Jumping ->
                    model

                _ ->
                    initJump 1.3 <| animate Jumping model

        Duck ->
            case model.state of
                Jumping ->
                    model

                _ ->
                    animate Ducking model

        Kill ->
            initJump 0.7 <| animate Dead model

        Tick delta ->
            case model.state of
                Jumping ->
                    updateAirbourne delta model

                Dead ->
                    updateAirbourne delta model

                Running ->
                    animate Running model

                Ducking ->
                    animate Ducking model

                _ ->
                    model


hasLandedFromJumping : Model -> Bool
hasLandedFromJumping rex =
    rex.yPos >= 0 && rex.state == Jumping


initJump : Float -> Model -> Model
initJump force rex =
    { rex
        | yPos = -force
        , yVel = -force
    }


updateAirbourne : Time -> Model -> Model
updateAirbourne delta ({ yPos, yVel, state } as rex) =
    let
        gravity =
            0.005

        ( state_, yPos_, yVel_ ) =
            if (hasLandedFromJumping rex) then
                ( Running, 0, 0 )
            else
                ( state, yPos + yVel * delta, yVel + gravity * delta )
    in
        { rex
            | yPos = yPos_
            , yVel = yVel_
            , state = state_
        }


animate : State -> Model -> Model
animate state ({ runCount, frameInc } as model) =
    let
        size =
            case state of
                Ducking ->
                    sizeDucking

                Dead ->
                    sizeDucking

                _ ->
                    sizeRunning
    in
        { model
            | state = state
            , width = size.width
            , height = size.height
            , runCount = runCount + frameInc
            , frameInc = 1 - frameInc
        }


hitDetected : Model -> List Cactus.Model -> Bool
hitDetected rex obstacles =
    case obstacles of
        [] ->
            False

        elem :: rest ->
            let
                { xMin, xMax, yMin, yMax } =
                    bounds rex

                { exMin, exMax, eyMin, eyMax } =
                    Cactus.bounds elem

                ( margin, marginAfter ) =
                    if rex.state == Jumping then
                        ( 25, 20 )
                    else
                        ( 4, 10 )

                xInBounds =
                    margin < xMax - exMin && marginAfter < exMax - xMin

                yInBounds =
                    margin < yMax - eyMin && margin < eyMax - yMin
            in
                if xInBounds && yInBounds then
                    True
                else
                    hitDetected rex rest


view : Model -> Svg Msg
view rex =
    let
        { xMin, yMin } =
            bounds rex
    in
        Svg.image
            [ x <| toString xMin
            , y <| toString yMin
            , width <| toString rex.width
            , height <| toString rex.height
            , xlinkHref <| render rex
            ]
            []


render : Model -> String
render { state, yVel, runCount } =
    let
        runningIndex =
            toString <| runCount % 6

        airbourneIndex =
            toString <|
                if yVel < 0.2 then
                    0
                else
                    1

        toImg action =
            "images/rex/" ++ action ++ ".png"
    in
        case state of
            Idle ->
                toImg <| "idle"

            Running ->
                toImg <| "run_" ++ runningIndex

            Jumping ->
                toImg <| "jump_" ++ airbourneIndex

            Ducking ->
                toImg <| "duck_" ++ runningIndex

            Dead ->
                toImg <| "dead_" ++ airbourneIndex


sizeRunning : { width : Float, height : Float }
sizeRunning =
    { width = 92, height = 84 }


sizeDucking : { width : Float, height : Float }
sizeDucking =
    { width = 108, height = 60 }


bounds : Model -> { xMin : Float, xMax : Float, yMin : Float, yMax : Float }
bounds rex =
    let
        ( offsetX, offsetY ) =
            ( offsetFromLeftEdge, windowHeight - offsetFromBottomEdge + rex.yPos )
    in
        { xMin = offsetX
        , xMax = offsetX + rex.width
        , yMin = offsetY - rex.height
        , yMax = offsetY
        }


offsetFromLeftEdge : Float
offsetFromLeftEdge =
    50


offsetFromBottomEdge : Float
offsetFromBottomEdge =
    60
