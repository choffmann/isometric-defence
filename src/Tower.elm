module Tower exposing (Tower)

import Point exposing (Point)



-- attackSpeed in Angriffe pro Sekunde


type alias Tower =
    { position : Point
    , price : Int
    , damage : Int
    , attackRadius : Int
    , attackSpeed : Float
    }
