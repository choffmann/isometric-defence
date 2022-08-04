module Messages exposing (..)

import Browser.Dom exposing (Element, Error(..), getElement)
import Json.Decode as Decode exposing (Decoder)
import Point exposing (Point)
import Task


type Key
    = Space
    | UnknownKey


type Msg
    = Tick Float
    | Key Key
    | Click Point
    | Canvas (Maybe Element)


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

                _ ->
                    UnknownKey
    in
    Decode.field "key" Decode.string
        |> Decode.map toKey
        |> Decode.map Key


clickDecoder : Decoder Msg
clickDecoder =
    Decode.succeed Point
        |> apply (Decode.field "movementX" Decode.int)
        |> apply (Decode.field "movementY" Decode.int)
        |> Decode.map Click


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.succeed Point
        |> apply (Decode.field "pageX" Decode.int)
        |> apply (Decode.field "pageY" Decode.int)
        |> Decode.map Click


getCanvas : Cmd Msg
getCanvas =
    Task.attempt
        (\result ->
            Canvas
                (case result of
                    Err _ ->
                        Nothing

                    Ok element ->
                        Just element
                )
        )
        (getElement "canvas")
