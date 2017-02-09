import Rex
import Cactus
import MovingElement as Elem

import Html exposing (Html, div, program, map)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, fill, fontFamily, textAnchor, xlinkHref)
import AnimationFrame

main : Program Never Model Msg
main = program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

-- Model

type alias Model = { rex: Rex.Model
                   , cacti: List Cactus.Model
                   }

init : (Model, Cmd Msg)
init = let cacti = [Cactus.init 400
                   ,Cactus.init 800
                   ,Cactus.init 1000
                   ]
       in (Model Rex.init cacti, Cmd.none)


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
    Tick delta      -> ({ model | rex = Rex.update (Rex.Tick delta) model.rex
                                , cacti = moveCacti delta model.cacti
                                }, Cmd.none)
    SubMsg          -> (model, Cmd.none)


moveCacti : Float -> List Cactus.Model -> List Cactus.Model
moveCacti delta cacti = case cacti of
  []   -> []
  h::t -> if h.xPos < 0
          then Cactus.init 900 :: moveCacti delta t
          else Cactus.update delta h :: moveCacti delta t

codeToMsg : KeyCode -> Rex.Msg
codeToMsg code =
  case code of
    40 -> Rex.Duck
    38 -> Rex.Jump
    _  -> Rex.Run

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ AnimationFrame.diffs Tick
            , Keyboard.downs KeyPressed
            , Keyboard.ups (\_ -> KeyReleased)
            ]


-- View

view : Model -> Html Msg
view ({rex, cacti} as model) =
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
                      , map (\_ -> SubMsg) (Rex.view windowSize rex)
                      ] ++ (renderElements windowSize cacti)

  in  Svg.svg svgAttributes sceneElements

renderElements : (Int,Int) -> List Elem.Model -> List (Svg Msg)
renderElements windowSize =
  List.map (\o -> map (\_ -> SubMsg) (Elem.view windowSize o))

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
  let y_ = h - 5 |> toString
  in  Svg.rect [ fill "#811"
               , x "0"
               , y y_
               , width (toString w)
               , height (toString h)
               ]
               []
