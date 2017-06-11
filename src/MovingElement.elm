module MovingElement exposing (Model, Msg, update, view, bounds)

import WindowSize exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


-- Model


type alias Model a =
    { a
        | xPos : Float
        , yPos : Float
        , width : Float
        , height : Float
        , img : String
    }



-- Update


type Msg
    = Tick Float


update : Float -> Model a -> Model a
update delta model =
    { model | xPos = model.xPos + scrollSpeed * delta }



-- View


view : Model a -> Svg Msg
view elem =
    let
        { exMin, eyMin } =
            bounds elem
    in
        -- Svg.rect
        --     [ fill "#F00"
        --     , x <| toString exMin
        --     , y <| toString eyMin
        --     , width <| toString elem.width
        --     , height <| toString elem.height
        --     ]
        --     []
        Svg.image
            [ x <| toString exMin
            , y <| toString eyMin
            , width <| toString elem.width
            , height <| toString elem.height
            , xlinkHref elem.img
            ]
            []


scrollSpeed : Float
scrollSpeed =
    -0.4


bounds : Model a -> { exMin : Float, exMax : Float, eyMin : Float, eyMax : Float }
bounds elem =
    let
        ( offsetX, offsetY ) =
            ( elem.xPos, windowHeight - offsetFromBottomEdge + elem.yPos )
    in
        { exMin = offsetX
        , exMax = offsetX + elem.width
        , eyMin = offsetY - elem.height
        , eyMax = offsetY
        }


offsetFromBottomEdge : Float
offsetFromBottomEdge =
    100
