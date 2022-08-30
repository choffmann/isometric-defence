module Enemy exposing (Enemies(..), Enemy, enemyList)

import Area exposing (Field(..))
import Point exposing (Point)


type alias Enemy =
    { id : Int
    , position : Field
    , hp : Int
    , speed : Float
    , worth : Int
    , damage : Int
    , distance : Float
    , enemyType : Enemies
    }


type Enemies
    = RedBox
    | BlueBox
    | YellowBox
    | WoodenBox
    | MetalBox


enemyList : List Enemy
enemyList =
    [ toEnemy WoodenBox 1 -100
    , toEnemy RedBox 2 -200
    , toEnemy BlueBox 3 -300
    , toEnemy YellowBox 4 -400
    , toEnemy MetalBox 5 -500
    ]


toEnemy : Enemies -> Int -> Float -> Enemy
toEnemy enemies id distance =
    let
        enemy hp speed worth damage =
            Enemy id (Field (Point -9999 -9999)) hp speed worth damage distance enemies
    in
    case enemies of
        WoodenBox ->
            enemy 1 8 1 1

        RedBox ->
            enemy 2 3 2 2

        BlueBox ->
            enemy 3 4 3 3

        YellowBox ->
            enemy 4 5 4 4

        MetalBox ->
            enemy 20 2 10 20
