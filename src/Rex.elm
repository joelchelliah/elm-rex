module Rex exposing ( Model, init, update, view )

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
           | Dead

init : Model
init = Model (image 100 100 "images/rex.jpg") Idle


-- Update

type Msg = Run
         | Jump
         | Kill

update : Msg -> Model -> Model
update msg model =
  case msg of
    Run  -> model
    Jump -> model
    Kill -> model


-- View

view : Model -> Html Msg
view model = div [id "rex"] [toHtml model.img]
