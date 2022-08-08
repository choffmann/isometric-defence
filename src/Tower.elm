module Tower exposing (Tower, Towers(..), toTower)

import Point exposing (Point)


type Towers
    = Basic


type alias Tower =
    { position : Point
    , price : Int
    , damage : Int
    , attackRadius : Int
    , attackSpeed : Float
    , lastShot : Float
    }


toTower : Towers -> Tower
toTower towers =
    case towers of
        Basic ->
            Tower (Point 0 0) 100 10 20 10000 100
