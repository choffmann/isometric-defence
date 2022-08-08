module Model exposing (Flags, GameState(..), Model, init)

import Browser.Dom exposing (Element)
import Enemy exposing (Enemy, toEnemy)
import FullScreenMode exposing (FullScreenMode)
import Messages exposing (Msg(..))
import Path exposing (Path, pointGenerator, testPath)
import Point exposing (Point)
import Random
import Tower exposing (Tower, toTower)


type GameState
    = Running
    | Paused
    | Won
    | Lost
    | GeneratePath


type alias Model =
    { gameState : GameState
    , hp : Int
    , money : Int
    , enemies : List Enemy
    , towers : List Tower
    , delta : Float
    , placingTower : Maybe Tower
    , canvas : Maybe Element
    , clicked : Maybe Point
    , fullscreen : FullScreenMode
    , speedMulti : Float
    , path : Maybe Path
    }


type alias Flags =
    { msg : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gameState = GeneratePath
      , hp = 1000
      , money = 0
      , enemies = [ toEnemy Enemy.Soldat, toEnemy Enemy.Soldat, toEnemy Enemy.Soldat, toEnemy Enemy.Soldat, toEnemy Enemy.Soldat ]
      , towers = [ toTower Tower.Basic, toTower Tower.Basic ]
      , delta = 0
      , placingTower = Nothing
      , canvas = Nothing
      , clicked = Nothing
      , fullscreen = FullScreenMode.Close
      , speedMulti = 1.0
      , path = Nothing
      }
    , Random.generate PathPointGenerate pointGenerator
    )
