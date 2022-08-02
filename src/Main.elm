module Main exposing (main)

import Browser
import Enemy exposing (Enemy)
import Html exposing (Html, text)
import Tower exposing (Tower)


type Msg
    = Msg


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
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    text (Debug.toString model.gameState)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
