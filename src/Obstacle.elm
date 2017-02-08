module Obstacle exposing ( Model, init, update, view)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, xlinkHref)
--import Element exposing (Element)
--import Time exposing (Time)

-- Model

type alias Model = { xPos : Float
                   , yPos: Float
                   , xVel: Float
                   }

init : Float -> Model
init x = Model x 0 -0.4


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
                , width "100"
                , height "100"
                , xlinkHref "images/cactus_1.png"
                ]
                []
