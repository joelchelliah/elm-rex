module Background exposing (view)

import List exposing (map, map2)
import List.Extra exposing (iterate)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import WindowSize exposing (..)


view : Svg {}
view =
    Svg.svg []
        [ renderSky
        , renderGround
        ]


renderSky : Svg {}
renderSky =
    let
        fillSkyRow =
            fillRow "0.08" "#4575FF"

        rows =
            map fillSkyRow skyRowPositions
    in
    Svg.svg [] rows


renderGround : Svg {}
renderGround =
    let
        fillGroundRow =
            fillRow "1.0"

        outline =
            fillGroundRow "#EEC39A" 90

        makeRow =
            map fillGroundRow groundRowColors

        rows =
            map2 identity makeRow groundRowPositions
    in
    Svg.svg [] <| outline :: rows


fillRow : String -> String -> Float -> Svg {}
fillRow alpha col yPos =
    Svg.rect
        [ fill col
        , x "0"
        , y <| String.fromFloat <| windowHeight - yPos
        , width <| String.fromFloat windowWidth
        , height <| String.fromFloat yPos
        , opacity alpha
        ]
        []


groundRowColors : List String
groundRowColors =
    [ "#D9A066"
    , "#CE975F"
    , "#C18C57"
    , "#B58452"
    , "#A87A4C"
    , "#9A7046"
    , "#8C6545"
    , "#825E41"
    , "#78563C"
    ]


skyRowPositions : List Float
skyRowPositions =
    let
        nextLine h =
            if h < 90 then
                Nothing

            else
                Just <| h - 15
    in
    iterate nextLine windowHeight


groundRowPositions : List Float
groundRowPositions =
    let
        nextLine h =
            if h < 0 then
                Nothing

            else
                Just (h - 10)
    in
    iterate nextLine 88
