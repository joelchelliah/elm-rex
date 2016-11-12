module Rex exposing ( Model, Msg, init, update, view,
                      run, duck, jump)

import Html exposing (..)
import Html.Attributes exposing (..)
import Element exposing (..)

-- Model

type alias Model = { img : Element
                   , state : State
                   }

type State = Idle
           | Running
           | Jumping
           | Ducking
           | Dead

init : Model
init = Model runningImg Idle

runningImg : Element
runningImg = (image 100 100 "images/rex.jpg")

duckingImg : Element
duckingImg = (image 100 40 "images/rex.jpg")

-- Update

type Msg = Run
         | Jump
         | Duck
         | Kill

update : Msg -> Model -> Model
update msg model =
  case msg of
    Run  -> { model | img = runningImg
                    , state = Running
                    }
    Jump -> model
    Duck -> { model | img = duckingImg
                    , state = Ducking
                    }
    Kill -> model


-- View

view : Model -> Html Msg
view model = div [id "rex"] [toHtml model.img]


-- Actions
run : Msg
run = Run

duck : Msg
duck = Duck

jump : Msg
jump = Jump
