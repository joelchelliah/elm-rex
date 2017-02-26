import Rex
import CactusGenerator as CactusGen
import DirtGenerator as DirtGen
import MovingElement as Elem

import Html exposing (Html, programWithFlags, div, map)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import AnimationFrame
import Random exposing (initialSeed)

main : Program Flags Model Msg
main = programWithFlags { init = init
                        , view = view
                        , update = update
                        , subscriptions = subscriptions
                        }

-- Model

type GameState = New
               | Playing
               | Paused
               | End

type alias Model = { state: GameState
                   , rex: Rex.Model
                   , cactusGen: CactusGen.Model
                   , dirtGen: DirtGen.Model
                   }

type alias Flags = { randomSeed : Int }

init : Flags -> (Model, Cmd Msg)
init {randomSeed} =
  let seed0 = initialSeed randomSeed
      model = { state     = New
              , rex       = Rex.init
              , cactusGen = CactusGen.init window.width seed0
              , dirtGen   = DirtGen.init window.width
              }
  in (model, Cmd.none)


-- Update

type Msg = Tick Time
         | KeyPressed KeyCode
         | KeyReleased
         | SubMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Playing -> (updatePlaying msg model, Cmd.none)
    _       -> (updatePaused msg model, Cmd.none)

updatePaused : Msg -> Model -> Model
updatePaused msg model =
  case msg of
    KeyPressed 32 -> { model | state = Playing }
    _ ->             model

updatePlaying : Msg -> Model -> Model
updatePlaying msg model =
  case msg of
    KeyPressed 32 ->
        { model | state = Paused }
    KeyPressed code ->
        { model | rex = Rex.update (codeToMsg code) model.rex }
    KeyReleased ->
      { model | rex = Rex.update Rex.Run model.rex }
    Tick delta ->
      { model | rex = Rex.update (Rex.Tick delta) model.rex
              , cactusGen = CactusGen.update delta model.cactusGen
              , dirtGen = DirtGen.update delta model.dirtGen }
    SubMsg ->
      model

codeToMsg : KeyCode -> Rex.Msg
codeToMsg code =
  case code of
    40 -> Rex.Duck
    38 -> Rex.Jump
    _  -> Rex.Run


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ AnimationFrame.diffs Tick
            , Keyboard.downs KeyPressed
            , Keyboard.ups (\_ -> KeyReleased)
            ]


-- View

view : Model -> Html Msg
view ({state, rex, cactusGen, dirtGen} as model) =
  let (w, h) = (window.width, window.height)
      windowSize = (w, h)
      attributes = [ width   <| toString w
                   , height  <| toString h
                   , viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
                   , version "1.1"
                   , style   "position: fixed;"
                   ]
      sceneElements = [ renderSky windowSize
                      , renderBackupGround windowSize
                      --, renderMovingElements windowSize dirtGen.dirtTiles
                      , renderMovingElements windowSize cactusGen.cacti
                      , renderRex windowSize rex
                      , renderMessage windowSize state
                      ]

  in  Svg.svg attributes sceneElements


renderMessage : (Float, Float) -> GameState -> Svg Msg
renderMessage (w, h) state =
  let yMiddle  = h / 2
      attrBase = [ x << toString <| w / 2
                 , textAnchor "middle"
                 , fill "#C12"
                 ]
      attrLarge yPos = [ y << toString <| yMiddle + yPos
                       , fontSize "60"
                       ] ++ attrBase
      attrSmall yPos = [ y << toString <| yMiddle + yPos
                       , fontSize "18"
                       ] ++ attrBase
  in case state of
    New ->
      Svg.svg [] [ Svg.text_ (attrLarge -50) [ Svg.text "RAWЯ!" ]
                 , Svg.text_ (attrSmall  0) [ Svg.text "Play using the arrow keys: ↑ ↓" ]
                 , Svg.text_ (attrSmall  35) [ Svg.text "Press SPACE to pause" ]
                 ]
    Paused ->
      Svg.svg [] [ Svg.text_ (attrLarge -20) [ Svg.text "Paused!" ]
                 , Svg.text_ (attrSmall  15) [ Svg.text "Press SPACE to continue" ]
                 ]
    End ->
      Svg.svg [] [ Svg.text_ (attrLarge -20) [ Svg.text "Game Ovər!" ]
                 , Svg.text_ (attrSmall  15) [ Svg.text "Press SPACE to try again" ]
                 ]
    Playing ->
      Svg.svg [][]

renderSky: (Float, Float) -> Svg Msg
renderSky (w, h) =
  Svg.rect [ fill "#99E"
           , x "0"
           , y "0"
           , width (toString w)
           , height (toString h) ]
           []

-- For when the ground sprites don't get rendered in time...
renderBackupGround: (Float,Float) -> Svg Msg
renderBackupGround (w, h) =
  let fillRect col yPos = Svg.rect [ fill col
                                   , x "0"
                                   , y <| toString <| window.height - yPos
                                   , width <| toString w
                                   , height <| toString yPos
                                   ]
                                   []
      gColors  = ["#EEC39A", "#D9A066", "#CE975F", "#C18C57",
                  "#B58452", "#A87A4C", "#9A7046"]
      gHeights = [90, 88, 72, 56, 40, 24, 8]
      filledRows = List.map fillRect gColors
      placedRows = List.map2 identity filledRows gHeights

  in Svg.svg [] placedRows

renderMovingElements : (Float, Float) -> List (Elem.Model a) -> Svg Msg
renderMovingElements windowSize elems =
  let render elem = map (\_ -> SubMsg) (Elem.view windowSize elem)
  in Svg.svg [] <| List.map render elems

renderRex : (Float, Float) -> Rex.Model -> Svg Msg
renderRex windowSize rex =
  map (\_ -> SubMsg) (Rex.view windowSize rex)

window : {width: Float, height: Float}
window = { width = 1000
         , height = 400
         }
