port module Main exposing (main)

import Area exposing (Area)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onMouseMove)
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemies(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Messages exposing (Key(..), Msg(..))
import Model exposing (Flags, GameState(..), Model)
import Styling
import Tower exposing (Towers(..))
import Update.Canvas as Canvas
import Update.Click as Click
import Update.FullScreenChange as FullScreenChange
import Update.Key as Key
import Update.Tick as Tick
import Utils.Decoder as Decoder


port enterFullScreen : () -> Cmd msg


port closeFullScreen : () -> Cmd msg


port fullScreenChangeReceiver : (Bool -> msg) -> Sub msg


canvas : Model -> Area -> List Renderable
canvas model area =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat area.width) (toFloat area.width) ]
    , shapes [ fill (Color.rgba 255 0 0 1) ] [ rect ( 0, 0 ) 100 50 ]
    ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (Debug.toString model) ]
        , div Styling.canvasContainerStyles
            [ div
                Styling.canvasStyles
                [ Canvas.toHtml
                    ( Area.area.width, Area.area.height )
                    [ id "canvas" ]
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
            Key.update key (enterFullScreen ()) (closeFullScreen ())

        Click point ->
            Click.update point

        Canvas maybe ->
            Canvas.update maybe

        FullScreenChange isFullScreen ->
            FullScreenChange.update isFullScreen


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        alwaysSubscribed =
            [ onAnimationFrameDelta Tick
            , onKeyDown Decoder.keyDecoder
            , onClick Decoder.clickDecoder
            , fullScreenChangeReceiver (\isFullScreen -> FullScreenChange isFullScreen)
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
