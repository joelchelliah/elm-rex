module Rex exposing ( Model, Msg(..), init, update, view,
                      run, duck, jump)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, xlinkHref)
import Element exposing (Element)
import Time exposing (Time)

-- Model

type alias Model = { state : State
                   , yPos: Float
                   , yVel: Float
                   }

type State = Idle
           | Running
           | Jumping
           | Ducking
           | Dead

init : Model
init = Model Idle 0 0


-- Update

type Msg = Run
         | Jump
         | Duck
         | Kill
         | Tick Float

update : Msg -> Model -> Model
update msg model =
  let isJumping = model.state == Jumping
  in case msg of
    Run  -> if isJumping
            then model
            else { model | state = Running }
    Jump -> if isJumping
            then model
            else { model | state = Jumping
                         , yPos  = -1
                         , yVel  = -1 }
    Duck -> if isJumping
            then model
            else { model | state = Ducking }
    Kill -> model

    Tick delta -> if isJumping
                  then move delta model
                  else model

move : Time -> Model -> Model
move delta ({yPos, yVel} as model) =
  let gravity = 0.005
      (state', yPos', yVel') = if yPos >= 0
                               then (Running, 0, 0)
                               else (Jumping, yPos + yVel * delta, yVel + gravity * delta)
  in { model | yPos = yPos'
             , yVel = yVel'
             , state = state' }




-- View

view : (Int,Int) -> Model -> Svg Msg
view (w, h) rex =
  let x' = 50 |> toString
      y' = (toFloat h) - 110 + (rex.yPos) |> toString
  in  Svg.image [ x x'
                , y y'
                , width "100"
                , height "100"
                , xlinkHref <| render rex
                ]
                []

render: Model -> String
render {state} = case state of
    Idle    -> "images/idle.jpg"
    Running -> "images/idle.jpg"
    Jumping -> "images/idle.jpg"
    Ducking -> "images/duck.jpg"
    Dead    -> "images/idle.jpg"


-- Actions
run : Msg
run = Run

duck : Msg
duck = Duck

jump : Msg
jump = Jump
