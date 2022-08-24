module Utils.Decoder exposing (keyDecoder, leftClickDecoder, mouseMoveDecoder, onContextMenuDecoder, receiveEventDecoder)

import Area exposing (Field(..))
import FullScreenMode exposing (FullScreenMode(..))
import GameView exposing (GameView(..))
import Json.Decode as Decode exposing (Decoder)
import Messages exposing (GameArea(..), Key(..), Msg, ReceivingEvents(..))
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


clearToCanvas : Model -> Point -> ( Maybe Pixel, GameArea )
clearToCanvas model point =
    let
        newCoordsToCanvas x y canvas =
            if x > round canvas.element.width || x < 0 || y > round canvas.element.height || y < 0 then
                Nothing

            else
                Just (Pixel { x = x, y = y })

        coordToCanvas canvas =
            newCoordsToCanvas (point.x - round canvas.element.x) (point.y - round canvas.element.y) canvas
    in
    case ( model.playCanvas, model.toolCanvas ) of
        ( Nothing, Nothing ) ->
            ( Nothing, PlayArea )

        ( Just playCanvas, Just toolCanvas ) ->
            case ( coordToCanvas playCanvas, coordToCanvas toolCanvas ) of
                ( Nothing, Nothing ) ->
                    ( Nothing, PlayArea )

                ( Just newCoords, _ ) ->
                    ( Just newCoords, PlayArea )

                ( Nothing, Just newCoords ) ->
                    ( Just newCoords, ToolArea )

        ( Just playCanvas, Nothing ) ->
            ( coordToCanvas playCanvas, PlayArea )

        ( Nothing, Just toolCanvas ) ->
            ( coordToCanvas toolCanvas, PlayArea )


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


coordinateDecoder : Model -> Decoder ( Maybe Pixel, GameArea )
coordinateDecoder model =
    case model.gameView of
        TopDown ->
            Decode.succeed Point
                |> apply (Decode.field "pageX" Decode.int)
                |> apply (Decode.field "pageY" Decode.int)
                |> Decode.map (clearToCanvas model)

        -- hier muss noch dazu in welchen Canvas man geklickt hat und dementsprechend andere Sachen machen
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
        |> Decode.map (\( maybePixel, gameArea ) -> Messages.LeftClick gameArea maybePixel)


mouseMoveDecoder : Model -> Decoder Msg
mouseMoveDecoder model =
    coordinateDecoder model
        |> Decode.map (\( maybePixel, gameArea ) -> Messages.MovePosition gameArea maybePixel)


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
