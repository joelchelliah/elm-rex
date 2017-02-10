module Rex exposing ( Model, Msg(..), init, update, view,
                      run, duck, jump)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, xlinkHref)
import Time exposing (Time)

-- Model

type alias Model = { state : State
                   , yPos: Float
                   , yVel: Float
                   , width: Float
                   , height: Float
                   , runCount : Int
                   , runInc : Int
                   }

type State = Idle
           | Running
           | Jumping
           | Ducking
           | Dead

init : Model
init = Model Idle 0 0 w h 1 0


-- Update

type Msg = Run
         | Jump
         | Duck
         | Kill
         | Tick Float

update : Msg -> Model -> Model
update msg model =
  let isJumping = model.state == Jumping
      isRunning = model.state == Running
      jumpForce = -1.3
  in case msg of
    Run  -> if isJumping
            then model
            else running model
    Jump -> if isJumping
            then model
            else { model | state = Jumping
                         , yPos  = jumpForce
                         , yVel  = jumpForce }
    Duck -> if isJumping
            then model
            else { model | state = Ducking }
    Kill -> model

    Tick delta -> if isJumping
                  then move delta model
                  else if isRunning
                       then running model
                       else model

move : Time -> Model -> Model
move delta ({yPos, yVel} as model) =
  let gravity = 0.005
      (state_, yPos_, yVel_) = if yPos >= 0
                               then (Running, 0, 0)
                               else (Jumping, yPos + yVel * delta, yVel + gravity * delta)
  in { model | yPos = yPos_
             , yVel = yVel_
             , state = state_ }

running : Model -> Model
running ({runCount, runInc} as model) =
  { model | state = Running
          , runCount = runCount + runInc
          , runInc = 1 - runInc }


-- View

view : (Float, Float) -> Model -> Svg Msg
view (_, windowH) rex =
  let (offsetX, offsetY) = (50, windowH - 60)
      x_ = offsetX |> toString
      y_ = offsetY - rex.height + rex.yPos |> toString
  in  Svg.image [ x x_
                , y y_
                , width <| toString rex.width
                , height <| toString rex.height
                , xlinkHref <| render rex
                ]
                []

render: Model -> String
render {state, yVel, runCount} =
  let runI = toString <| runCount % 6
      jumpI = toString <| if yVel < 0 then 0 else 1
  in case state of
    Idle    -> "images/idle.png"
    Running -> "images/run_" ++ runI ++ ".png"
    Jumping -> "images/jump_" ++ jumpI ++ ".png"
    Ducking -> "images/run_3.png"
    Dead    -> "images/idle.png"


-- Actions

run : Msg
run = Run

duck : Msg
duck = Duck

jump : Msg
jump = Jump


w : Float
w = size.width

h : Float
h = size.height

size : {width: Float, height: Float}
size = { width = 92, height = 84 }
