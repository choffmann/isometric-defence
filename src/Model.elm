module Model exposing (Flags, GameState(..), Model, init)

import Browser.Dom exposing (Element)
import Enemy exposing (Enemy, toEnemy)
import FullScreenMode exposing (FullScreenMode)
import Messages exposing (Msg)
import Point exposing (Point)
import Tower exposing (Tower, toTower)


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
    , placingTower : Maybe Tower
    , canvas : Maybe Element
    , clicked : Maybe Point
    , fullscreen : FullScreenMode
    }


type alias Flags =
    { msg : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gameState = Paused
      , hp = 1000
      , money = 0
      , enemies = [ toEnemy Enemy.Soldat, toEnemy Enemy.Soldat, toEnemy Enemy.Soldat, toEnemy Enemy.Soldat, toEnemy Enemy.Soldat ]
      , towers = [ toTower Tower.Basic, toTower Tower.Basic ]
      , delta = 0
      , placingTower = Nothing
      , canvas = Nothing
      , clicked = Nothing
      , fullscreen = FullScreenMode.Close
      }
    , Cmd.none
    )
