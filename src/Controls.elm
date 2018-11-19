module Controls exposing (Key(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Key
    = Up
    | Down
    | Space


decoder : Decoder Key
decoder =
    Decode.field "key" Decode.string
        |> Decode.andThen decodeDirection


decodeDirection : String -> Decoder Key
decodeDirection key =
    case key of
        "ArrowUp" ->
            Decode.succeed Up

        "ArrowDown" ->
            Decode.succeed Down

        " " ->
            Decode.succeed Space

        _ ->
            Decode.fail "Not a valid Direction key."
