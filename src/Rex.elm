module Rex exposing ( Model, Msg(..), init, update, view,
                      run, duck, jump)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, xlinkHref)
--import Element exposing (Element)
import Time exposing (Time)

-- Model

type alias Model = { state : State
                   , yPos: Float
                   , yVel: Float
                   , width: Int
                   , height: Int
                   , runCounter : Int
                   , runIncs : List Int
                   }

type State = Idle
           | Running
           | Jumping
           | Ducking
           | Dead

init : Model
init =
  let (w, h) = (92, 84)
      runCounter = 1
      runIncs = [1, 0, 0]
  in Model Idle 0 0 w h runCounter runIncs


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
  in case msg of
    Run  -> if isJumping
            then model
            else running model
    Jump -> if isJumping
            then model
            else { model | state = Jumping
                         , yPos  = -1.2
                         , yVel  = -1.2 }
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
running model =
  let headInc = Maybe.withDefault 0 (List.head model.runIncs)
      restInc = List.drop 1 model.runIncs
  in { model | state = Running
             , runCounter = model.runCounter + headInc
             , runIncs = restInc ++ [headInc]}


-- View

view : (Int, Int) -> Model -> Svg Msg
view (w, h) rex =
  let x_ = 50 |> toString
      y_ = (toFloat h) - 110 + (rex.yPos) |> toString
  in  Svg.image [ x x_
                , y y_
                , width <| toString rex.width
                , height <| toString rex.height
                , xlinkHref <| render rex
                ]
                []

render: Model -> String
render {state, runCounter} = case state of
    Idle    -> "images/idle.png"
    Running -> "images/run_" ++ (toString (runCounter % 5 + 1)) ++ ".png"
    Jumping -> "images/jump.png"
    Ducking -> "images/run_3.png"
    Dead    -> "images/idle.png"


-- Actions
run : Msg
run = Run

duck : Msg
duck = Duck

jump : Msg
jump = Jump