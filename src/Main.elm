import Rex
import Cactus
import GroundTile
import MovingElement as Elem

import Html exposing (Html, div, program, map)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, fill, fontFamily, fontSize, textAnchor, xlinkHref)
import AnimationFrame

main : Program Never Model Msg
main = program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

-- Model

type GameState = Playing
               | Paused

type alias Model = { state: GameState
                   , rex: Rex.Model
                   , cacti: List Cactus.Model
                   , ground: List GroundTile.Model
                   }

init : (Model, Cmd Msg)
init =
  let cacti  = List.map Cactus.init [300, 800, 1100]
      tilesX = List.map ((*) GroundTile.w << toFloat) <| List.range 0 5
      ground = List.map GroundTile.init <| tilesX
  in (Model Paused Rex.init cacti ground, Cmd.none)


-- Update

type Msg = Tick Time
         | KeyPressed KeyCode
         | KeyReleased
         | SubMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Paused  -> (updatePaused msg model, Cmd.none)
    Playing -> (updatePlaying msg model, Cmd.none)

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
                     , cacti = moveCacti delta model.cacti
                     , ground = moveGround delta model.ground }
    SubMsg ->
      model


moveCacti : Float -> List Cactus.Model -> List Cactus.Model
moveCacti delta cacti = case cacti of
  []   -> []
  h::t -> let moveTail = moveCacti delta t
          in if h.xPos < -h.width
             then Cactus.init window.width :: moveTail
             else Cactus.update delta h :: moveTail

moveGround : Float -> List GroundTile.Model -> List GroundTile.Model
moveGround delta ground = case ground of
    []   -> []
    h::t -> let errorMargin = 10
                moveTail = moveGround delta t
            in if h.xPos < -h.width
               then GroundTile.init (window.width - errorMargin) :: moveTail
               else GroundTile.update delta h :: moveTail

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
      svgAttributes = [ width (toString w)
                      , height (toString h)
                      , Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
                      -- , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
                      , Attributes.version "1.1"
                      , Attributes.style "position: fixed;"
                      ]
      sceneElements = [ renderSky windowSize
                      , renderBackupGround windowSize
                      ] ++ (renderElements windowSize ground)
                        ++ (renderElements windowSize cacti)
                        ++ [ map (\_ -> SubMsg) (Rex.view windowSize rex)
                           , renderMessage windowSize state
                           ]

  in  Svg.svg svgAttributes sceneElements


renderMessage : (Float, Float) -> GameState -> Svg Msg
renderMessage (w, h) state =
  case state of
    Paused ->
      let yMiddle  = h / 2
          attrBase = [ x << toString <| w / 2
                     , textAnchor "middle"
                     , fill "#E24"
                     ]
          attrLarge = [ y << toString <| yMiddle - 20
                      , fontSize "50"
                      ] ++ attrBase
          attrSmall = [ y << toString <| yMiddle + 15
                      , fontSize "18"
                      ] ++ attrBase
      in Svg.svg [] [ Svg.text_ attrLarge [ Svg.text "Paused" ]
                    , Svg.text_ attrSmall [ Svg.text "Press SPACE to continue" ]
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

renderElements : (Float, Float) -> List Elem.Model -> List (Svg Msg)
renderElements windowSize =
  List.map (\o -> map (\_ -> SubMsg) (Elem.view windowSize o))

window : {width: Float, height: Float}
window = { width = 1000
         , height = 400
         }
