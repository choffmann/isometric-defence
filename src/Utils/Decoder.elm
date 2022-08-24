module Utils.Decoder exposing (keyDecoder, leftClickDecoder, mouseMoveDecoder, onContextMenuDecoder, receiveEventDecoder)

import Area exposing (Field(..))
import FullScreenMode exposing (FullScreenMode(..))
import GameView exposing (GameView(..))
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

                "i" ->
                    I

                "I" ->
                    I

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
    let
        newCoordsToCanvas x y =
            if x > (Area.area.width + Area.fieldSize) || x < 0 || y > Area.area.height || y < 0 then
                Nothing

            else
                Just (Pixel { x = x, y = y })
    in
    model.canvas
        |> Maybe.andThen
            (\canvas ->
                newCoordsToCanvas (point.x - round canvas.element.x) (point.y - round canvas.element.y)
            )


clearIso : Model -> Point -> Maybe Point
clearIso model point =
    let
        newCoordsToCanvas x y =
            if x < 0 || y < 0 then
                Nothing

            else
                Just (Point x y |> calcOffset)
    in
    model.canvas
        |> Maybe.andThen
            (\canvas ->
                newCoordsToCanvas (point.x - round canvas.element.x) (point.y - round canvas.element.y)
            )


maybePixelToPoint : GameView -> Maybe Pixel -> Maybe Point
maybePixelToPoint gameView pixel =
    pixel
        |> Maybe.map Area.pixelToField
        |> Maybe.map (\(Field point) -> point)


clear : Model -> Point -> Maybe Point
clear model point =
    model.canvas
        |> Maybe.andThen
            (\canvas ->
                Just (Point (point.x - round canvas.element.x) (point.y - round canvas.element.y) |> calcOffset)
            )


calcOffset : Point -> Point
calcOffset { x, y } =
    { x = x - (Area.isometricArea.width // 2), y = y }


check : Maybe Point -> Maybe Point
check point =
    point
        |> Maybe.andThen
            (\{ x, y } ->
                if x < 0 || x > Area.widthTiles - 1 || y < 0 || y > Area.heightTiles - 1 then
                    Nothing

                else
                    Just (Point x y)
            )


coordinateDecoder : Model -> Decoder (Maybe Point)
coordinateDecoder model =
    case model.gameView of
        TopDown ->
            Decode.succeed Point
                |> apply (Decode.field "pageX" Decode.int)
                |> apply (Decode.field "pageY" Decode.int)
                |> Decode.map (clearToCanvas model)
                |> Decode.map (maybePixelToPoint model.gameView)

        Isometric ->
            Decode.succeed Point
                |> apply (Decode.field "pageX" Decode.int)
                |> apply (Decode.field "pageY" Decode.int)
                --|> Decode.map (clearIso model)
                |> Decode.map (clear model)
                |> Decode.map (Maybe.map Area.isometricToPoint)
                |> Decode.map check


leftClickDecoder : Model -> Decoder Msg
leftClickDecoder model =
    coordinateDecoder model
        |> Decode.map Messages.LeftClick


mouseMoveDecoder : Model -> Decoder Msg
mouseMoveDecoder model =
    coordinateDecoder model
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


type alias CustomEventDecoder =
    { message : Msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }


onContextMenuDecoder : Decoder CustomEventDecoder
onContextMenuDecoder =
    Decode.succeed CustomEventDecoder
        |> apply (Decode.succeed Messages.RightClick)
        |> apply (Decode.succeed True)
        |> apply (Decode.succeed True)
