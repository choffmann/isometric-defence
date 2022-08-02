module Main exposing (Model, main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Point, Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemy)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Point exposing (Point)
import Tower exposing (Tower)


type Msg
    = Tick Float


type alias Area =
    { width : Int
    , height : Int
    }


constarea : Area
constarea =
    Area 750 750


type GameState
    = Running
    | Paused
    | Won
    | Lost


type alias Model =
    { gameState : GameState
    , hp : Int
    , money : Int
    , enemies : List Enemy
    , towers : List Tower
    , delta : Float
    }


type alias Flags =
    { msg : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gameState = Paused
      , hp = 1000
      , money = 0
      , enemies = []
      , towers = []
      , delta = 0
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
        [ div [] [ text (Debug.toString model.gameState) ]
        , div [] [ text (String.fromFloat model.delta) ]
        , div [ style "display" "flex", style "justify-content" "center", style "align-items" "center" ]
            [ Canvas.toHtml ( constarea.width, constarea.height ) [ style "border" "10px solid black" ] (canvas model constarea) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model | delta = delta }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Tick


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
