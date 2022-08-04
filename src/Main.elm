module Main exposing (main)

import Area exposing (Area)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onMouseMove)
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemies(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import Messages exposing (Key(..), Msg(..))
import Model exposing (Flags, GameState(..), Model)
import Styles
import Tower exposing (Towers(..))
import Update.Canvas as Canvas
import Update.Click as Click
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.Key as Key
import Update.Tick as Tick
import Utils.Decoder as Decoder
import Utils.Ports as Ports


canvas : Model -> Area -> List Renderable
canvas model area =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat area.width) (toFloat area.width) ]
    , shapes [ fill (Color.rgba 255 0 0 1) ] [ rect ( 0, 0 ) 100 50 ]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        alwaysSubscribed =
            [ onAnimationFrameDelta Tick
            , onKeyDown Decoder.keyDecoder
            , onClick Decoder.clickDecoder
            , Ports.onFullScreenChange
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
