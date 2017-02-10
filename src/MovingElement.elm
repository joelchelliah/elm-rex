module MovingElement exposing (Model, Msg, init, update, view)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, xlinkHref)


-- Model

type alias Model = { xPos : Float
                   , yPos: Float
                   , width: Float
                   , height: Float
                   , img: String
                   }

init : Float -> Float -> Float -> Float -> String -> Model
init xPos yPos w h img =
  Model xPos yPos w h img


-- Update

type Msg = Tick Float

update : Float -> Model -> Model
update delta model =
  { model | xPos = model.xPos + scrollSpeed * delta }


-- View

view : (Float, Float) -> Model -> Svg Msg
view (_, windowH) model =
  let (offsetX, offsetY) = (0, windowH - 100)
      x_ = offsetX + model.xPos |> toString
      y_ = offsetY - model.yPos |> toString
  in  Svg.image [ x x_
                , y y_
                , width <| toString model.width
                , height <| toString model.height
                , xlinkHref model.img
                ]
                []


scrollSpeed : Float
scrollSpeed = -0.4
