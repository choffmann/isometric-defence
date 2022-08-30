module Tower exposing (Tower, Towers(..), toTower)

import Point exposing (Point)


type Towers
    = Basic
    | Tower1
    | Tower2
    | Tower3
    | Tower4


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
    let
        tower price damage attackRadius attackSpeed =
            Tower (Point -9999 -9999) price damage attackRadius attackSpeed 0 towers
    in
    case towers of
        Basic ->
            tower 100 1 3 800

        Tower1 ->
            tower 200 1 4 400

        Tower2 ->
            tower 300 20 5 1000

        Tower3 ->
            tower 400 3 5 500

        Tower4 ->
            tower 500 1 10 200
