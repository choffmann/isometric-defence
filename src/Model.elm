module Model exposing (Flags, GameState(..), Model, PlacingTower, init, restart)

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


type alias PlacingTower =
    { tower : Tower
    , canBePlaced : Bool
    }


type alias Model =
    { gameState : GameState
    , hp : Int
    , money : Int
    , enemies : List Enemy
    , towers : List Tower
    , delta : Float
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
    }


type alias Flags =
    { msg : String }


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
    ( { gameState = GeneratePath
      , hp = 1000
      , money = 1000
      , enemies = [ Enemy.toEnemy Enemy.Soldat ] --, Enemy.toEnemy Enemy.Soldat, Enemy.toEnemy Enemy.Soldat, Enemy.toEnemy Enemy.Soldat, Enemy.toEnemy Enemy.Soldat ]
      , towers = [] --, toTower Tower.Basic ]
      , delta = 0
      , placingTower = Just { tower = Tower.toTower Tower.Basic, canBePlaced = False }
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
      }
    , Random.generate PathPointGenerate Path.pointGenerator
    )
