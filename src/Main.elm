module Main exposing (Model, main)

import Area exposing (Area)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, Shape, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemy)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Path exposing (Path, PathPoint, pathSize, testPath)
import Point exposing (Point)
import Tower exposing (Tower)


type Msg
    = Tick Float
    | BuildPath


constarea : Area
constarea =
    Area 10 10


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


pointToCanvas : Point -> Shape
pointToCanvas point =
    rect ( toFloat (point.x * pathSize), toFloat (point.y * pathSize) ) (toFloat pathSize) (toFloat pathSize)


pathToCanvas : Path -> Renderable
pathToCanvas path =
    shapes [ fill (Color.rgb255 255 50 50) ] (List.map (\pathPoint -> pointToCanvas pathPoint.point) path)


canvas : Model -> Area -> List Renderable
canvas model area =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat area.width) (toFloat area.height) ]
    , pathToCanvas testPath
    ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (Debug.toString model.gameState) ]
        , div [] [ text (String.fromFloat model.delta) ]
        , div [] [ button [ onClick BuildPath ] [ text "Build Path" ] ]
        , div [ style "display" "flex", style "justify-content" "center", style "align-items" "center" ]
            [ Canvas.toHtml ( constarea.width * pathSize, constarea.height * pathSize ) [ style "border" "10px solid black" ] (canvas model constarea) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model | delta = delta }, Cmd.none )

        BuildPath ->
            Debug.todo "Build Path"


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
