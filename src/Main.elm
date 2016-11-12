import Rex as Rex

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, millisecond)
import Keyboard exposing (KeyCode)
import Char exposing (fromCode)
import String exposing (fromChar)


main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }

-- Model

type alias Model = { rex: Rex.Model
                   , obstacles : List String
                   }

init : (Model, Cmd Msg)
init = (Model Rex.init [], Cmd.none)


-- Update

type Msg = Tick
         | KeyPressed KeyCode
         | KeyReleased

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyPressed code -> ({model | rex = Rex.update (codeToMsg code) model.rex}, Cmd.none)
    KeyReleased     -> ({model | rex = Rex.update Rex.run model.rex}, Cmd.none)
    Tick            -> (model, Cmd.none)


codeToMsg : KeyCode -> Rex.Msg
codeToMsg code =
  case code of
    40 -> Rex.duck
    38 -> Rex.jump
    _  -> Rex.run

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ Time.every 1000 (\_ -> Tick)
            , Keyboard.downs KeyPressed
            , Keyboard.ups (\_ -> KeyReleased)
            ]


-- View

view : Model -> Html Msg
view {rex, obstacles} =
  let viewRex = App.map (\_ -> Tick) (Rex.view rex)
  in div [id "main"]
         [viewRex]
