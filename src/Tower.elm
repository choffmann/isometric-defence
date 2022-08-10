module Tower exposing (Tower, Towers(..), toTower)

import Point exposing (Point)


type Towers
    = Basic


type alias Tower =
    { position : Point
    , price : Int
    , damage : Int
    , attackRadius : Float
    , attackSpeed : Float
    , lastShot : Float
    }


toTower : Towers -> Tower
toTower towers =
    case towers of
        Basic ->
            Tower (Point 2 3) 100 10 3 100 100
