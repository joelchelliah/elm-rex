module Cloud exposing (Model, init, update, view)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import WindowSize exposing (windowWidth)


type alias Model =
    { xPos : Float
    , speed : Float
    }


init : Model
init =
    { xPos = 700
    , speed = 0.02
    }


update : Float -> Model -> Model
update delta ({ xPos, speed } as cloud) =
    let
        ( leftEdge, rightEdge ) =
            ( 0, windowWidth - size.width )
    in
    if xPos < leftEdge then
        { cloud | xPos = leftEdge, speed = -speed }

    else if xPos > rightEdge then
        { cloud | xPos = rightEdge, speed = -speed }

    else
        { cloud | xPos = cloud.xPos - cloud.speed * delta }


view : Model -> Svg {}
view cloud =
    Svg.image
        [ x <| String.fromFloat cloud.xPos
        , y <| String.fromFloat 50
        , width <| String.fromFloat size.width
        , height <| String.fromFloat size.height
        , opacity "0.3"
        , xlinkHref "images/clouds/cloud_0.png"
        ]
        []


size : { width : Float, height : Float }
size =
    { width = 152
    , height = 84
    }
