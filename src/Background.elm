module Background exposing (view)

import WindowSize exposing (..)

import List exposing (map, map2)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


view : Svg {}
view =
  Svg.svg [] [ renderSky
             , renderGround
             ]


renderSky: Svg {}
renderSky =
  Svg.rect [ fill "#99E"
           , x "0"
           , y "0"
           , width  <| toString windowWidth
           , height <| toString windowHeight
           ]
           []


renderGround : Svg {}
renderGround =
  let filledRows = map fillRect groundRowColors
      placedRows = map2 identity filledRows groundRowPositions
  in Svg.svg [] placedRows

fillRect : String -> Float -> Svg {}
fillRect col yPos =
  Svg.rect [ fill col
           , x "0"
           , y <| toString <| windowHeight - yPos
           , width  <| toString windowWidth
           , height <| toString yPos
           ]
           []

groundRowColors : List String
groundRowColors =
  let line = "#EEC39A"
  in line :: ["#D9A066", "#CE975F", "#C18C57", "#B58452", "#A87A4C", "#9A7046"]

groundRowPositions : List Float
groundRowPositions =
  let line = 90
  in line :: [88, 72, 56, 40, 24, 8]
