module Cloud exposing (Model, init, update, view)

import WindowSize exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


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
        [ x <| toString cloud.xPos
        , y <| toString 50
        , width <| toString size.width
        , height <| toString size.height
        , opacity "0.3"
        , xlinkHref "images/clouds/cloud_0.png"
        ]
        []


size : { width : Float, height : Float }
size =
    { width = 152
    , height = 84
    }
