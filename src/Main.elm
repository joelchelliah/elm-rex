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

type alias Model = { rex: String
                   , obstacles : List String
                   }
                   
init : (Model, Cmd Msg)
init = (Model "RAWR! (todo)" [], Cmd.none)


-- Update

type Msg = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> (model, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Time.every second Tick


-- View

view : Model -> Html Msg
view model = div [id "main-container"]
                 [text (toString model)]
