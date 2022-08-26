module Tower exposing (Tower, Towers(..), toTower)

import Point exposing (Point)


type Towers
    = Basic
    | Tower1
    | Tower2
    | Tower3


type alias Tower =
    { position : Point
    , price : Int
    , damage : Int
    , attackRadius : Float
    , attackSpeed : Float
    , lastShot : Float
    , towerType : Towers
    }


toTower : Towers -> Tower
toTower towers =
    case towers of
        Basic ->
            Tower (Point 3 3) 100 1 3 100 100 Basic

        Tower1 ->
            Tower (Point 3 3) 200 0 3 100 100 Tower1

        Tower2 ->
            Tower (Point 3 3) 300 0 3 100 100 Tower2

        Tower3 ->
            Tower (Point 3 3) 400 0 3 100 100 Tower3
