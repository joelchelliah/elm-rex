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
                   , frameInc : Int
                   }

type State = Idle
           | Running
           | Jumping
           | Ducking
           | Dead

init : Model
init = { state = Idle
       , yPos = 0
       , yVel = 0
       , width = sizeRunning.width
       , height = sizeRunning.width
       , runCount = 1
       , frameInc = 0
       }


-- Update

type Msg = Run
         | Jump
         | Duck
         | Kill
         | Tick Float

update : Msg -> Model -> Model
update msg model =
  case msg of
    Run  -> case model.state of
      Jumping -> model
      _       -> animate Running model
    Jump -> case model.state of
      Jumping -> model
      _       -> initJump <| animate Running model
    Duck -> case model.state of
      Jumping -> model
      _       -> animate Ducking model
    Kill -> { model | state = Dead }

    Tick delta -> case model.state of
      Jumping -> updateJump delta model
      Running -> animate Running model
      Ducking -> animate Ducking model
      _       -> model

initJump : Model -> Model
initJump model =
  let jumpForce = -1.3
  in { model | state = Jumping
             , yPos  = jumpForce
             , yVel  = jumpForce }

updateJump : Time -> Model -> Model
updateJump delta ({yPos, yVel} as model) =
  let gravity = 0.005
      (state_, yPos_, yVel_) = if yPos >= 0
                               then (Running, 0, 0)
                               else (Jumping, yPos + yVel * delta, yVel + gravity * delta)
  in { model | yPos = yPos_
             , yVel = yVel_
             , state = state_ }

animate : State -> Model -> Model
animate state ({runCount, frameInc} as model) =
  let size = case state of
    Ducking -> sizeDucking
    _       -> sizeRunning
  in { model | state = state
             , width = size.width
             , height = size.height
             , runCount = runCount + frameInc
             , frameInc = 1 - frameInc }


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
  let runningIndex = toString <| runCount % 6
      jumpingIndex = toString <| if yVel < 0 then 0 else 1
      toImg action = "images/" ++ action ++ ".png"
  in case state of
    Idle    -> toImg <| "idle"
    Running -> toImg <| "run_" ++ runningIndex
    Jumping -> toImg <| "jump_" ++ jumpingIndex
    Ducking -> toImg <| "duck_" ++ runningIndex
    Dead    -> toImg <| "idle"


-- Actions

run : Msg
run = Run

duck : Msg
duck = Duck

jump : Msg
jump = Jump


sizeRunning : {width: Float, height: Float}
sizeRunning = { width = 92, height = 84 }

sizeDucking : {width: Float, height: Float}
sizeDucking = { width = 108, height = 60 }
