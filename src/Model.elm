module Model exposing (FiredShot, Flags, GameState(..), Model, PlacingTower, Round(..), init, restart)

import Area exposing (Field)
import Browser.Dom exposing (Element)
import Enemy exposing (Enemy)
import FullScreenMode exposing (FullScreenMode)
import GameView exposing (GameView(..))
import Messages exposing (Msg(..))
import Path exposing (Path)
import Point exposing (Point)
import Random
import Screen exposing (Screen(..))
import Sprite exposing (IsometricViewSprite, Sprite, TowerAreaSprite)
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
    , clicked : Maybe Point
    , fullscreen : FullScreenMode
    , speedMulti : Float
    , path : Maybe Path
    , sprite : Load Sprite
    , gameView : GameView
    , movePosition : Maybe Point
    , screen : Screen
    , animation : Maybe Animation
    , shotsFired : List FiredShot
    , round : Round
    }


type alias Flags =
    { msg : String }


type Round
    = Round1
    | Round2
    | Round3
    | Round4
    | Round5
    | Round6
    | Round7


restart : Model -> Flags -> ( Model, Cmd Msg )
restart model flags =
    let
        newModel ( initModel, command ) =
            ( { initModel | playCanvas = model.playCanvas, toolCanvas = model.toolCanvas, fullscreen = model.fullscreen, speedMulti = model.speedMulti }, command )
    in
    init flags
        |> newModel


init : Flags -> ( Model, Cmd Msg )
init _ =
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
      , path = Just Path.testPath
      , sprite = Loading
      , gameView = TopDown
      , movePosition = Nothing
      , screen = StartScreen
      , animation = Nothing
      , shotsFired = []
      }
    , Cmd.none
    )
