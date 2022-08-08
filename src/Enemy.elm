module Enemy exposing (Enemies(..), Enemy, toEnemy)

import Area exposing (Field(..))
import Point exposing (Point)


type alias Enemy =
    { position : Field
    , hp : Int
    , speed : Int
    , worth : Int
    , damage : Int
    , distance : Int
    }


type Enemies
    = Soldat


toEnemy : Enemies -> Enemy
toEnemy enemies =
    case enemies of
        Soldat ->
            Enemy (Field (Point 1 2)) 2 3 4 5 0
