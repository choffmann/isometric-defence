module Main exposing (main)

import Area
import Browser
import Browser.Events
import Canvas exposing (Renderable)
import Canvas.Settings
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import Messages exposing (Msg(..))
import Model exposing (Flags, Model)
import Styles
import Ui.DrawUtils as DrawUtils
import Ui.Enemy
import Ui.Path
import Ui.Tower
import Update.Canvas as Canvas
import Update.Click as Click
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.GeneratePath as GeneratePath
import Update.Key as Key
import Update.Tick as Tick
import Utils.Decoder as Decoder
import Utils.Ports as Ports


canvas : Model -> List Renderable
canvas model =
    [ Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid Area.area Area.fieldSize
    , Ui.Path.pathToCanvas model.path
    , Ui.Enemy.enemiesToCanvas model.enemies model.path
    , Ui.Tower.towersToCanvas model.towers
    , Ui.Tower.towerRadius model.towers
    ]


view : Model -> Html Msg
view model =
    div (id "app" :: Styles.appContainer)
        [ div []
            [ div [] [ text (String.fromFloat model.delta) ]

            -- TODO: Remove Debug.toString
            -- , div [] [ text (Debug.toString { model | delta = 0 }) ]
            ]
        , div Styles.canvasContainerStyles
            [ div
                (onMouseEnter Messages.EnterCanvas :: id "canvasContainer" :: Styles.canvasStyles Area.area)
                [ Canvas.toHtml
                    ( Area.area.width, Area.area.height )
                    []
                    (canvas model)
                ]
            ]
        , div Styles.canvasContainerStyles
            [ div
                (onMouseEnter Messages.EnterCanvas :: id "canvasContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                [ Canvas.toHtml
                    ( Ui.Tower.towerArea.width, Ui.Tower.towerArea.height )
                    []
                    Ui.Tower.towerCanvas
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

        PathDirectionGenerate direction ->
            GeneratePath.update (PathDirectionGenerate direction)

        PathPointGenerate point ->
            GeneratePath.update (PathPointGenerate point)


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
