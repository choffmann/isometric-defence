module Tower exposing (Tower, Towers(..), findTowerById, toTower)

import Area exposing (Field(..))
import Point exposing (Point)


type Towers
    = Basic
    | Gun
    | Cannon
    | Sniper
    | Minigun


type alias Tower =
    { id : Int
    , position : Field
    , price : Int
    , damage : Int
    , attackRadius : Float
    , attackSpeed : Float
    , lastShot : Float
    , towerType : Towers
    }


toTower : Int -> Towers -> Tower
toTower id towers =
    let
        tower price damage attackRadius attackSpeed =
            Tower id (Field (Point -9999 -9999)) price damage attackRadius attackSpeed 0 towers
    in
    case towers of
        Basic ->
            tower 100 1 3 800

        Gun ->
            tower 200 1 4 400

        Cannon ->
            tower 300 20 5 1000

        Sniper ->
            tower 400 3 5 500

        Minigun ->
            tower 500 1 10 200


findTowerById : Int -> List Tower -> Maybe Tower
findTowerById id towers =
    case towers of
        [] ->
            Nothing

        h :: hs ->
            if id == h.id then
                Just h

            else
                findTowerById id hs
