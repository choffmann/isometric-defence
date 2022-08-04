module Utils.Decoder exposing (clickDecoder, keyDecoder, mouseMoveDecoder)

import Json.Decode as Decode exposing (Decoder)
import Messages exposing (Key(..), Msg)
import Point exposing (Point)


apply : Decoder a -> Decoder (a -> b) -> Decoder b
apply =
    Decode.map2 (|>)


keyDecoder : Decoder Msg
keyDecoder =
    let
        toKey k =
            case k of
                " " ->
                    Space

                "F" ->
                    F

                "f" ->
                    F

                _ ->
                    UnknownKey
    in
    Decode.field "key" Decode.string
        |> Decode.map toKey
        |> Decode.map Messages.Key


clickDecoder : Decoder Msg
clickDecoder =
    Decode.succeed Point
        |> apply (Decode.field "pageX" Decode.int)
        |> apply (Decode.field "pageY" Decode.int)
        |> Decode.map Messages.Click


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.succeed Point
        |> apply (Decode.field "movementX" Decode.int)
        |> apply (Decode.field "movementY" Decode.int)
        |> Decode.map Messages.Click
