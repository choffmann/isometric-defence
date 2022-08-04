module Main exposing (main)

import Area exposing (Area)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onMouseMove)
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemies(..), toEnemy)
import Html exposing (Html, div, text)
import Messages exposing (Key(..), Msg(..))
import Model exposing (GameState(..), Model)
import Styling
import Tower exposing (Towers(..), toTower)
import Update.Canvas as Canvas
import Update.Click as Click
import Update.Key as Key
import Update.Tick as Tick


type alias Flags =
    { msg : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gameState = Paused
      , hp = 1000
      , money = 0
      , enemies = [ toEnemy Soldat, toEnemy Soldat, toEnemy Soldat, toEnemy Soldat, toEnemy Soldat ]
      , towers = [ toTower Basic, toTower Basic ]
      , delta = 0
      , placingTower = Nothing
      , canvas = Nothing
      }
    , Cmd.none
    )


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


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        alwaysSubscribed =
            [ onAnimationFrameDelta Tick
            , onKeyDown Messages.keyDecoder
            , onClick Messages.clickDecoder
            ]
    in
    Sub.batch
        (case model.placingTower of
            Just tower ->
                onMouseMove Messages.mouseMoveDecoder :: alwaysSubscribed

            Nothing ->
                alwaysSubscribed
        )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
