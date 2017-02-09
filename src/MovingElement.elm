module MovingElement exposing (Model, Msg, init, update, view)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, xlinkHref)


-- Model

type alias Model = { xPos : Float
                   , yPos: Float
                   , xVel: Float
                   , width: Int
                   , height: Int
                   , img: String
                   }

init : Float -> Float -> Int -> Int -> String -> Model
init xPos yPos w h img =
  Model xPos yPos -0.4 w h img


-- Update

type Msg = Tick Float

update : Float -> Model -> Model
update delta ({xPos, xVel} as model) =
  { model | xPos = xPos + xVel * delta }


-- View

view : (Int,Int) -> Model -> Svg Msg
view (w, h) model =
  let x_ = model.xPos |> toString
      y_ = (toFloat h) - 110 + (model.yPos) |> toString
  in  Svg.image [ x x_
                , y y_
                , width <| toString model.width
                , height <| toString model.height
                , xlinkHref model.img
                ]
                []
