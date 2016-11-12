import Rex as Rex

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick -> (model, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Time.every second (\_ -> Tick)


-- View

view : Model -> Html Msg
view {rex, obstacles} =
  let viewRex = App.map (\_ -> Tick) (Rex.view rex)
  in div [id "main"]
         [viewRex]
