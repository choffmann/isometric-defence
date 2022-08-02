module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Enemy exposing (Enemy)
import Html exposing (Html, div, text)
import Tower exposing (Tower)


type Msg
    = Tick Float


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


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (Debug.toString model.gameState) ]
        , div [] [ text (String.fromFloat model.delta) ]
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
