import Rex
import Cactus
import GroundTile
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
                   , cacti: List Cactus.Model
                   , ground: List GroundTile.Model
                   }

type alias Flags = { randomSeed : Int }

init : Flags -> (Model, Cmd Msg)
init {randomSeed} =
  --let cacti  = List.map Cactus.init [300, 800, 1100]
  let seed0  = initialSeed randomSeed
      cacti  = [Cactus.init 400 seed0]
      tilesX = List.map ((*) GroundTile.w << toFloat) <| List.range 0 3
      ground = List.map GroundTile.init <| tilesX
  in (Model New Rex.init cacti ground, Cmd.none)


-- Update

type Msg = Tick Time
         | KeyPressed KeyCode
         | KeyReleased
         | SubMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Playing -> (updatePlaying msg model, Cmd.none)
    _ -> (updatePaused msg model, Cmd.none)

updatePaused : Msg -> Model -> Model
updatePaused msg model =
  case msg of
    KeyPressed 32 ->
      { model | state = Playing }
    _ ->
      model

updatePlaying : Msg -> Model -> Model
updatePlaying msg model =
  case msg of
    KeyPressed 32 ->
        { model | state = Paused }
    KeyPressed code ->
        { model | rex = Rex.update (codeToMsg code) model.rex }
    KeyReleased ->
      { model | rex = Rex.update Rex.run model.rex }
    Tick delta ->
      { model | rex = Rex.update (Rex.Tick delta) model.rex
              , cacti = Cactus.move window.width delta model.cacti
              , ground = GroundTile.move window.width delta model.ground }
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
view ({state, rex, cacti, ground} as model) =
  let (w, h) = (window.width, window.height)
      windowSize = (w, h)
      attributes = [ width (toString w)
                   , height (toString h)
                   , viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
                   , version "1.1"
                   , style "position: fixed;"
                   ]
      sceneElements = [ renderSky windowSize
                      , renderBackupGround windowSize
                      ] ++ (renderMovingElements windowSize ground)
                        ++ (renderMovingElements windowSize cacti)
                        ++ [ map (\_ -> SubMsg) (Rex.view windowSize rex)
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
      Svg.svg [] [ Svg.text_ (attrLarge -20) [ Svg.text "Pauƨed!" ]
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
  let y_ = window.height - 86 |> toString
  in  Svg.rect [ fill "#C18C57"
               , x "0"
               , y y_
               , width (toString w)
               , height (toString h)
               ]
               []

renderMovingElements : (Float, Float) -> List (Elem.Model a) -> List (Svg Msg)
renderMovingElements windowSize =
  List.map (\o -> map (\_ -> SubMsg) (Elem.view windowSize o))

window : {width: Float, height: Float}
window = { width = 1000
         , height = 400
         }
