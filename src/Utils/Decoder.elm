module Utils.Decoder exposing (clickDecoder, keyDecoder, mouseMoveDecoder, receiveEventDecoder)

import Area exposing (Field(..), area, fieldSize)
import FullScreenMode exposing (FullScreenMode(..))
import Json.Decode as Decode exposing (Decoder)
import Messages exposing (Key(..), Msg, ReceivingEvents(..))
import Model exposing (Model)
import Pixel exposing (Pixel(..))
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

                "r" ->
                    R

                "R" ->
                    R

                "ArrowUp" ->
                    ArrowUp

                "ArrowDown" ->
                    ArrowDown

                _ ->
                    UnknownKey
    in
    Decode.field "key" Decode.string
        |> Decode.map toKey
        |> Decode.map Messages.Key


clearToCanvas : Model -> Point -> Maybe Pixel
clearToCanvas model point =
    model.canvas
        |> Maybe.andThen
            (\canvas ->
                case ( point.x - round canvas.element.x, point.y - round canvas.element.y ) of
                    ( newX, newY ) ->
                        if newX > (area.width + fieldSize) || newX < 0 || newY > area.height || newY < 0 then
                            Nothing

                        else
                            Just (Pixel { x = newX, y = newY })
            )


maybePixelToPoint : Maybe Pixel -> Maybe Point
maybePixelToPoint pixel =
    pixel
        |> Maybe.map Area.pixelToField
        |> Maybe.map (\(Field point) -> point)


clickDecoder : Model -> Decoder Msg
clickDecoder model =
    Decode.succeed Point
        |> apply (Decode.field "pageX" Decode.int)
        |> apply (Decode.field "pageY" Decode.int)
        |> Decode.map (clearToCanvas model)
        |> Decode.map maybePixelToPoint
        |> Decode.map Messages.Click


mouseMoveDecoder : Model -> Decoder Msg
mouseMoveDecoder model =
    Decode.succeed Point
        |> apply (Decode.field "pageX" Decode.int)
        |> apply (Decode.field "pageY" Decode.int)
        |> Decode.map (clearToCanvas model)
        |> Decode.map maybePixelToPoint
        |> Decode.map Messages.MovePosition


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
