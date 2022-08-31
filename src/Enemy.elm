module Enemy exposing (Enemies(..), Enemy, findEnemyById, round1, round2, round3, round4, round5, round6, round7)

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
    = CardBoardBox
    | WoodenBox
    | RedBox
    | BlueBox
    | YellowBox
    | MetalBox
    | Palette


round1 : List Enemy
round1 =
    [ toEnemy RedBox 101 -100
    , toEnemy RedBox 102 -200
    , toEnemy RedBox 103 -300
    , toEnemy RedBox 104 -400
    , toEnemy RedBox 105 -500
    , toEnemy RedBox 106 -600
    , toEnemy RedBox 107 -700
    , toEnemy RedBox 108 -800
    , toEnemy RedBox 109 -900
    , toEnemy RedBox 110 -1000
    ]


round2 : List Enemy
round2 =
    [ toEnemy RedBox 201 -100
    , toEnemy RedBox 202 -200
    , toEnemy RedBox 203 -300
    , toEnemy BlueBox 204 -1000
    , toEnemy BlueBox 205 -1100
    , toEnemy BlueBox 206 -1200
    , toEnemy CardBoardBox 207 -3000
    , toEnemy BlueBox 208 -1300
    ]


round3 : List Enemy
round3 =
    [ toEnemy RedBox 301 -100
    , toEnemy RedBox 302 -150
    , toEnemy RedBox 303 -200
    , toEnemy BlueBox 304 -600
    , toEnemy BlueBox 305 -650
    , toEnemy BlueBox 306 -700
    , toEnemy YellowBox 307 -1500
    , toEnemy YellowBox 308 -1700
    , toEnemy YellowBox 309 -1900
    ]


round4 : List Enemy
round4 =
    [ toEnemy YellowBox 401 -100
    , toEnemy YellowBox 402 -200
    , toEnemy YellowBox 403 -300
    , toEnemy YellowBox 404 -400
    , toEnemy YellowBox 405 -500
    , toEnemy YellowBox 406 -600
    , toEnemy YellowBox 407 -700
    , toEnemy YellowBox 408 -800
    , toEnemy YellowBox 409 -900
    , toEnemy YellowBox 410 -1000
    , toEnemy CardBoardBox 411 -1000
    , toEnemy CardBoardBox 412 -1200
    , toEnemy WoodenBox 413 -1300
    , toEnemy WoodenBox 414 -1400
    ]


round5 : List Enemy
round5 =
    [ toEnemy WoodenBox 501 -1000
    , toEnemy WoodenBox 502 -1200
    , toEnemy MetalBox 503 -50
    , toEnemy MetalBox 504 -200
    , toEnemy BlueBox 505 -400
    , toEnemy BlueBox 506 -600
    , toEnemy BlueBox 507 -800
    , toEnemy CardBoardBox 508 -2000
    , toEnemy CardBoardBox 509 -2200
    ]


round6 : List Enemy
round6 =
    [ toEnemy RedBox 601 -100
    , toEnemy RedBox 602 -300
    , toEnemy RedBox 603 -500
    , toEnemy RedBox 604 -700
    , toEnemy RedBox 605 -900
    , toEnemy RedBox 606 -1100
    , toEnemy RedBox 607 -1300
    , toEnemy RedBox 608 -1500
    , toEnemy RedBox 609 -1700
    , toEnemy RedBox 610 -1900
    , toEnemy YellowBox 611 -200
    , toEnemy YellowBox 612 -400
    , toEnemy YellowBox 613 -600
    , toEnemy YellowBox 614 -800
    , toEnemy YellowBox 615 -1000
    , toEnemy YellowBox 616 -1200
    , toEnemy YellowBox 617 -1400
    , toEnemy YellowBox 618 -1600
    , toEnemy YellowBox 619 -1800
    , toEnemy YellowBox 620 -2000
    , toEnemy BlueBox 621 -150
    , toEnemy BlueBox 622 -300
    , toEnemy BlueBox 623 -450
    , toEnemy BlueBox 624 -600
    , toEnemy BlueBox 625 -750
    , toEnemy BlueBox 626 -900
    , toEnemy BlueBox 627 -1050
    , toEnemy BlueBox 628 -1200
    , toEnemy BlueBox 629 -1350
    , toEnemy BlueBox 630 -1500
    , toEnemy WoodenBox 631 -500
    , toEnemy WoodenBox 632 -1000
    , toEnemy WoodenBox 633 -1500
    , toEnemy WoodenBox 634 -2000
    , toEnemy WoodenBox 635 -2500
    , toEnemy MetalBox 636 -100
    , toEnemy MetalBox 637 -200
    , toEnemy MetalBox 638 -300
    , toEnemy MetalBox 639 -400
    , toEnemy MetalBox 640 -500
    , toEnemy CardBoardBox 641 -550
    , toEnemy CardBoardBox 642 -1050
    , toEnemy CardBoardBox 643 -1550
    , toEnemy CardBoardBox 644 -2050
    , toEnemy CardBoardBox 645 -2550
    ]


round7 : List Enemy
round7 =
    [ toEnemy Palette 701 -10 ]


toEnemy : Enemies -> Int -> Float -> Enemy
toEnemy enemies id distance =
    let
        enemy hp speed worth damage =
            Enemy id (Field (Point -9999 -9999)) hp speed worth damage distance enemies
    in
    case enemies of
        CardBoardBox ->
            enemy 1 10 1 0

        WoodenBox ->
            enemy 2 8 2 0

        RedBox ->
            enemy 3 3 3 0

        BlueBox ->
            enemy 4 4 4 0

        YellowBox ->
            enemy 5 5 5 0

        MetalBox ->
            enemy 20 2 10 0

        Palette ->
            enemy 1000 1 1000 1000


findEnemyById : Int -> List Enemy -> Maybe Enemy
findEnemyById id enemies =
    case enemies of
        [] ->
            Nothing

        h :: hs ->
            if id == h.id then
                Just h

            else
                findEnemyById id hs
