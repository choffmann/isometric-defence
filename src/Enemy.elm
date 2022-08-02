module Enemy exposing (Enemies(..), Enemy, toEnemy)

import Point exposing (Point)


type alias Enemy =
    { position : Point
    , hp : Int
    , speed : Int
    , worth : Int
    , damage : Int
    }


type Enemies
    = Soldat


toEnemy : Enemies -> Enemy
toEnemy enemies =
    case enemies of
        Soldat ->
            Enemy (Point 1 2) 2 3 4 5
