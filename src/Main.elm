module Main exposing (main)

import Area
import Browser
import Browser.Events
import Canvas exposing (Renderable, Shape)
import Canvas.Settings
import Color
import Enemy exposing (Enemy)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import List.Extra as List
import List.Nonempty as Nonempty
import Messages exposing (Msg(..))
import Model exposing (Flags, GameState(..), Model)
import Path exposing (Path, PathDirection(..), PathPoint)
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Random
import Styles
import Tower exposing (Tower)
import Update.Canvas as Canvas
import Update.Click as Click
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.GeneratePath as GeneratePath
import Update.Key as Key
import Update.Tick as Tick
import Utils.Decoder as Decoder
import Utils.Ports as Ports


pointToCanvas : Point -> Float -> Float -> Shape
pointToCanvas point width height =
    Canvas.rect ( toFloat (point.x * Area.fieldSize), toFloat (point.y * Area.fieldSize) ) width height


pathToCanvas : Maybe Path -> Renderable
pathToCanvas path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 255 50 50) ] (List.map (\pathPoint -> pointToCanvas pathPoint.point (toFloat Area.fieldSize) (toFloat Area.fieldSize)) (Nonempty.toList justPath))


enemiesToCanvas : List Enemy -> Maybe Path -> Renderable
enemiesToCanvas enemies path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            enemies
                |> List.map
                    (\enemy ->
                        Path.distanceToPixel justPath enemy.distance
                            |> Maybe.map
                                (\(Pixel point) ->
                                    Canvas.rect ( toFloat point.x - 10, toFloat point.y - 10 ) 20 20
                                )
                    )
                |> List.removeNothing
                |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 255 50) ]


towersToCanvas : List Tower -> Renderable
towersToCanvas towers =
    towers
        |> List.map
            (\tower ->
                pointToCanvas tower.position 20 20
            )
        |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 50 255) ]


canvas : Model -> List Renderable
canvas model =
    [ Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , pathToCanvas model.path
    , enemiesToCanvas model.enemies model.path
    , towersToCanvas model.towers
    ]


view : Model -> Html Msg
view model =
    div (id "app" :: Styles.appContainer)
        [ div []
            [ div [] [ text (String.fromFloat model.delta) ]

            -- TODO: Remove Debug.toString
            , div [] [ text (Debug.toString { model | delta = 0 }) ]
            ]
        , div Styles.canvasContainerStyles
            [ div
                (onMouseEnter Messages.EnterCanvas :: id "canvasContainer" :: Styles.canvasStyles)
                [ Canvas.toHtml
                    ( Area.area.width, Area.area.height )
                    []
                    (canvas model)
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
                            if GeneratePath.checkIsLastPoint path then
                                ( { model | gameState = Paused }, Cmd.none )

                            else
                                ( { model | path = Just (GeneratePath.createPoint model.path (Nonempty.last path) direction) }, Random.generate PathDirectionGenerate (Path.directionGenerator (GeneratePath.checkDirection model.path)) )

                _ ->
                    ( model, Cmd.none )

        PathPointGenerate point ->
            ( { model | path = Just (GeneratePath.createFirstRandomPoint (PathPoint point Right)) }, Random.generate PathDirectionGenerate (Path.directionGenerator (GeneratePath.checkDirection model.path)) )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        alwaysSubscribed =
            [ Browser.Events.onAnimationFrameDelta Tick
            , Browser.Events.onKeyDown Decoder.keyDecoder
            , Browser.Events.onClick (Decoder.clickDecoder model)
            , Ports.onEventMessage
            ]
    in
    Sub.batch
        (case model.placingTower of
            Just _ ->
                Browser.Events.onMouseMove Decoder.mouseMoveDecoder :: alwaysSubscribed

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
