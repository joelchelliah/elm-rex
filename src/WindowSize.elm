module WindowSize exposing (windowHeight, windowWidth)


type alias Model =
    { width : Float
    , height : Float
    }


windowHeight : Float
windowHeight =
    size.height


windowWidth : Float
windowWidth =
    size.width


size : Model
size =
    Model 1000 400
