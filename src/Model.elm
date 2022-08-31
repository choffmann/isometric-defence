module Model exposing (FiredShot, GameState(..), Model, PlacingTower, init, restart)

import Area exposing (Field)
import Browser.Dom exposing (Element)
import Enemy exposing (Enemy)
import FullScreenMode exposing (FullScreenMode)
import GameView exposing (GameView(..))
import Messages exposing (Msg)
import Path exposing (Path)
import Round exposing (Round(..))
import Screen exposing (Screen(..))
import Sprite exposing (Sprite)
import Tower exposing (Tower)
import Ui.Animation exposing (Animation)
import Utils.Data exposing (Load(..))


type GameState
    = Running
    | Paused
    | Won
    | Lost
    | GeneratePath
    | WaitToStart
    | StartScreenAnimation


type alias PlacingTower =
    { tower : Tower
    , canBePlaced : Bool
    }


type alias FiredShot =
    { enemyId : Int
    , towerId : Int
    , range : Float
    , distance : Float
    }


type alias Model =
    { gameState : GameState
    , hp : Int
    , money : Int
    , enemies : List Enemy
    , towers : List Tower
    , delta : Float
    , nextTowerId : Int
    , placingTower : Maybe PlacingTower
    , inspectingTower : Maybe Tower
    , playCanvas : Maybe Element
    , toolCanvas : Maybe Element
    , clicked : Maybe Field
    , fullscreen : FullScreenMode
    , speedMulti : Float
    , path : Maybe Path
    , sprite : Load Sprite
    , gameView : GameView
    , movePosition : Maybe Field
    , screen : Screen
    , animation : Maybe Animation
    , shotsFired : List FiredShot
    , round : Round
    }


restart : Model -> ( Model, Cmd Msg )
restart model =
    let
        newModel ( initModel, command ) =
            ( { initModel
                | playCanvas = model.playCanvas
                , toolCanvas = model.toolCanvas
                , fullscreen = model.fullscreen
                , speedMulti = model.speedMulti
                , gameView = model.gameView
              }
            , command
            )
    in
    init
        |> newModel


init : ( Model, Cmd Msg )
init =
    ( { gameState = StartScreenAnimation
      , hp = 100
      , money = 200
      , round = Round1
      , enemies = Enemy.round1
      , towers = []
      , nextTowerId = 1
      , delta = 0
      , placingTower = Nothing
      , inspectingTower = Nothing
      , playCanvas = Nothing
      , toolCanvas = Nothing
      , clicked = Nothing
      , fullscreen = FullScreenMode.Close
      , speedMulti = 1.0
      , path = Nothing
      , sprite = Loading
      , gameView = TopDown
      , movePosition = Nothing
      , screen = StartScreen
      , animation = Nothing
      , shotsFired = []
      }
    , Cmd.none
    )
