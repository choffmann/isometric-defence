module Model exposing (..)

import Browser.Dom exposing (Element)
import Enemy exposing (Enemy)
import Tower exposing (Tower)


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
    }
