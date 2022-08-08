module Main exposing (main)

import Area exposing (Area, fieldSize)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onMouseMove)
import Canvas exposing (Renderable, Shape, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemy)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import Messages exposing (Key(..), Msg(..))
import Model exposing (Flags, GameState(..), Model)
import Path exposing (Path, PathPoint, distanceToPixel, testPath)
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Styles
import Time
import Update.Canvas as Canvas
import Update.Click as Click
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.GeneratePath as GeneratePath
import Update.Key as Key
import Update.Tick as Tick
import Utils.Decoder as Decoder
import Utils.Ports as Ports


pointToCanvas : Point -> Shape
pointToCanvas point =
    rect ( toFloat (point.x * fieldSize), toFloat (point.y * fieldSize) ) (toFloat fieldSize) (toFloat fieldSize)


pathToCanvas : Path -> Renderable
pathToCanvas path =
    shapes [ fill (Color.rgb255 255 50 50) ] (List.map (\pathPoint -> pointToCanvas pathPoint.point) path)


enemiesToCanvas : List Enemy -> Path -> Renderable
enemiesToCanvas enemies path =
    enemies
        |> List.map
            (\enemy ->
                case Debug.log "Pixel" (distanceToPixel path enemy.distance) of
                    Pixel point ->
                        rect ( toFloat point.x - 10, toFloat point.y - 10 ) 20 20
            )
        |> shapes [ fill (Color.rgb255 50 255 50) ]


canvas : Model -> Area -> List Renderable
canvas model area =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat area.width) (toFloat area.height) ]
    , pathToCanvas model.path
    , enemiesToCanvas model.enemies model.path
    ]


view : Model -> Html Msg
view model =
    div (id "app" :: Styles.appContainer)
        [ div []
            [ div [] [ text (String.fromFloat model.delta) ]
            , div [] [ text (Debug.toString { model | delta = 0 }) ]
            ]
        , div Styles.canvasContainerStyles
            [ div
                (onMouseEnter Messages.EnterCanvas :: id "canvasContainer" :: Styles.canvasStyles)
                [ Canvas.toHtml
                    ( Area.area.width, Area.area.height )
                    []
                    (canvas model Area.area)
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        Tick delta ->
            Tick.update delta

        Key key ->
            Key.update key

        Click point ->
            Click.update point

        Canvas maybe ->
            Canvas.update maybe

        EnterCanvas ->
            EnterCanvas.update

        Event event ->
            Event.update event

        GeneratePath ->
            GeneratePath.update

        PathDirectionGenerate direction ->
            Debug.todo ""

        PathPointGenerate point ->
            Debug.todo ""



-- Time.every speed (\_ -> Tick)
-- onAnimationFrameDelta Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        alwaysSubscribed =
            [ Time.every 100 (\_ -> Tick 100)
            , onKeyDown Decoder.keyDecoder
            , onClick (Decoder.clickDecoder model)
            , Ports.onEventMessage
            ]
    in
    Sub.batch
        (case model.placingTower of
            Just tower ->
                onMouseMove Decoder.mouseMoveDecoder :: alwaysSubscribed

            Nothing ->
                alwaysSubscribed
        )


main : Program Flags Model Msg
main =
    Browser.element
        { init = Model.init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
