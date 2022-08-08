module Main exposing (main)

import Area exposing (Area, area, fieldSize)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onMouseMove)
import Canvas exposing (Renderable, Shape, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemy)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import List.Nonempty exposing (cons, last, length, map, singleton, toList)
import Messages exposing (Key(..), Msg(..))
import Model exposing (Flags, GameState(..), Model)
import Path exposing (Path, PathDirection(..), PathPoint, directionGenerator, distanceToPixel, testPath)
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Random
import Styles
import Time
import Update.Canvas as Canvas
import Update.Click as Click
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.GeneratePath as GeneratePath exposing (checkDirection, createFirstRandomPoint, createPoint)
import Update.Key as Key
import Update.Tick as Tick
import Utils.Decoder as Decoder
import Utils.Ports as Ports


pointToCanvas : Point -> Shape
pointToCanvas point =
    rect ( toFloat (point.x * fieldSize), toFloat (point.y * fieldSize) ) (toFloat fieldSize) (toFloat fieldSize)


pathToCanvas : Maybe Path -> Renderable
pathToCanvas path =
    case path of
        Nothing ->
            shapes [] []

        Just justPath ->
            shapes [ fill (Color.rgb255 255 50 50) ] (List.map (\pathPoint -> pointToCanvas pathPoint.point) (toList justPath))


enemiesToCanvas : List Enemy -> Maybe Path -> Renderable
enemiesToCanvas enemies path =
    case path of
        Nothing ->
            shapes [] []

        Just justPath ->
            enemies
                |> List.map
                    (\enemy ->
                        --case Debug.log "Pixel" (distanceToPixel justPath enemy.distance) of
                        case distanceToPixel justPath enemy.distance of
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
update msg model =
    case msg of
        Tick delta ->
            Tick.update delta model

        Key key ->
            Key.update key model

        Click point ->
            Click.update point model

        Canvas maybe ->
            Canvas.update maybe model

        EnterCanvas ->
            EnterCanvas.update model

        Event event ->
            Event.update event model

        Messages.GeneratePath ->
            GeneratePath.update model

        PathDirectionGenerate direction ->
            case model.gameState of
                Model.GeneratePath ->
                    case model.path of
                        Nothing ->
                            ( model, Cmd.none )

                        Just path ->
                            let
                                checkIsEnd : Bool
                                checkIsEnd =
                                    (last path).point.x + 1 > ((area.width // fieldSize) - 1)
                            in
                            if checkIsEnd then
                                ( { model | gameState = Paused }, Cmd.none )

                            else
                                ( { model | path = Just (createPoint model.path (last path) direction) }, Random.generate PathDirectionGenerate (directionGenerator (checkDirection model.path)) )

                _ ->
                    ( model, Cmd.none )

        PathPointGenerate point ->
            ( { model | path = Just (createFirstRandomPoint (PathPoint point Right)) }, Random.generate PathDirectionGenerate (directionGenerator (checkDirection model.path)) )



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
