import Rex exposing (Msg)

import Html exposing (Html, div)
import Html.App as App
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Time exposing (Time, millisecond)
import Keyboard exposing (KeyCode)
import Char exposing (fromCode)
import String exposing (fromChar)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, fill, fontFamily, textAnchor, xlinkHref)


main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }

-- Model

type alias Model = { rex: Rex.Model
                   , obstacles: List String
                   , time: Time
                   }

init : (Model, Cmd Msg)
init = (Model Rex.init [] 0, Cmd.none)


-- Update

type Msg = Tick Time
         | KeyPressed KeyCode
         | KeyReleased
         | SubMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyPressed code -> ({model | rex = Rex.update (codeToMsg code) model.rex}, Cmd.none)
    KeyReleased     -> ({model | rex = Rex.update Rex.run model.rex}, Cmd.none)
    Tick delta      -> ({model | rex = Rex.update (Rex.Tick delta) model.rex}, Cmd.none)
    SubMsg          -> (model, Cmd.none)


codeToMsg : KeyCode -> Rex.Msg
codeToMsg code =
  case code of
    40 -> Rex.duck
    38 -> Rex.jump
    _  -> Rex.run

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ Time.every 1000 Tick
            , Keyboard.downs KeyPressed
            , Keyboard.ups (\_ -> KeyReleased)
            ]


-- View

view : Model -> Html Msg
view ({rex, obstacles} as model) =
  let (w, h) = (800, 300)
      windowSize = (w, h)
      svgAttributes = [ width (toString w)
                      , height (toString h)
                      , Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
                      -- , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
                      , Attributes.version "1.1"
                      , Attributes.style "position: fixed;"
                      ]
      sceneElements = [ renderSky windowSize
                      , renderGround windowSize
                      , App.map (\_ -> SubMsg) (Rex.view windowSize rex)
                      ]
  in  Svg.svg svgAttributes sceneElements


renderSky: (Int,Int) -> Svg Msg
renderSky (w, h) =
  Svg.rect [ fill "#99E"
           , x "0"
           , y "0"
           , width (toString w)
           , height (toString h) ]
           []

renderGround: (Int,Int) -> Svg Msg
renderGround (w, h) =
  let y' = h - 5 |> toString
  in  Svg.rect [ fill "#811"
               , x "0"
               , y y'
               , width (toString w)
               , height (toString h)
               ]
               []
