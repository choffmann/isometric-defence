module Utils.Decoder exposing (clickDecoder, keyDecoder, mouseMoveDecoder, receiveEventDecoder)

import Area exposing (area)
import FullScreenMode exposing (FullScreenMode(..))
import Json.Decode as Decode exposing (Decoder)
import Messages exposing (Key(..), Msg, ReceivingEvents(..))
import Model exposing (Model)
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


clickDecoder : Model -> Decoder Msg
clickDecoder model =
    let
        clearToCanvas point =
            case model.canvas of
                Nothing ->
                    Nothing

                Just canvas ->
                    case ( point.x - round canvas.element.x, point.y - round canvas.element.y ) of
                        ( newX, newY ) ->
                            if newX > area.width || newX < 0 || newY > area.height || newY < 0 then
                                Nothing

                            else
                                Just { x = newX, y = newY }
    in
    Decode.succeed Point
        |> apply (Decode.field "pageX" Decode.int)
        |> apply (Decode.field "pageY" Decode.int)
        |> Decode.map clearToCanvas
        |> Decode.map Messages.Click


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.succeed Point
        |> apply (Decode.field "movementX" Decode.int)
        |> apply (Decode.field "movementY" Decode.int)
        |> Decode.map Just
        |> Decode.map Messages.Click


type alias EventMsg =
    { event : String
    , message : String
    }


receiveEventDecoder : Decoder ReceivingEvents
receiveEventDecoder =
    let
        toEvent msg =
            case msg.event of
                "fullScreenChanged" ->
                    case msg.message of
                        "opened" ->
                            FullScreenChanged Open

                        "closed" ->
                            FullScreenChanged Close

                        _ ->
                            UnknownEvent

                _ ->
                    UnknownEvent
    in
    Decode.succeed EventMsg
        |> apply (Decode.field "event" Decode.string)
        |> apply (Decode.field "message" Decode.string)
        |> Decode.map toEvent
