module Update.Tick exposing (update)

import Area exposing (Field)
import Enemy exposing (Enemy)
import Model exposing (GameState(..), Model)
import Path exposing (Path, distanceToPathPoint)
import Point exposing (Point)
import Tower exposing (Tower)


damageEnemies : Tower -> List Enemy -> ( Tower, List Enemy )
damageEnemies =
    let
        internal : List Enemy -> Tower -> List Enemy -> ( Tower, List Enemy )
        internal acc tower enemyList =
            case enemyList of
                [] ->
                    ( tower, List.sortBy (\enemy -> enemy.distance) acc )

                enemy :: hs ->
                    if tower.lastShot > tower.attackSpeed && inRange tower.position tower.attackRadius enemy.position then
                        internal (({ enemy | hp = enemy.hp - tower.damage } :: acc) ++ hs) { tower | lastShot = 0 } []

                    else
                        internal (enemy :: acc) tower hs
    in
    internal []


damage : List Tower -> List Enemy -> ( List Tower, List Enemy )
damage =
    let
        internal : List Tower -> List Tower -> List Enemy -> ( List Tower, List Enemy )
        internal acc towers enemies =
            case towers of
                [] ->
                    ( acc, enemies )

                tower :: hs ->
                    if tower.lastShot > tower.attackSpeed then
                        case damageEnemies tower enemies of
                            ( newTower, newEnemies ) ->
                                internal (newTower :: acc) hs newEnemies

                    else
                        internal (tower :: acc) hs enemies
    in
    internal []


killEnemies : List Enemy -> List Enemy
killEnemies =
    List.filter (\enemy -> enemy.hp > 0)


inRange : Point -> Int -> Field -> Bool
inRange poin1 radius point2 =
    True


moveEnemies : Path -> List Enemy -> List Enemy
moveEnemies path =
    List.map
        (\enemy ->
            { enemy
                | distance = enemy.distance + enemy.speed
                , position = distanceToPathPoint path (enemy.distance + enemy.speed)
            }
        )


cooldownTowers : Float -> List Tower -> List Tower
cooldownTowers delta =
    List.map (\tower -> { tower | lastShot = tower.lastShot + delta })


tick : Model -> Float -> Model
tick model delta =
    let
        moneyfromKilledEnemies enemies =
            List.foldl
                (\enemy money ->
                    if enemy.hp <= 0 then
                        money + enemy.worth

                    else
                        money
                )
                0
                enemies
    in
    case damage model.towers model.enemies of
        ( towers, enemies ) ->
            case enemies |> moveEnemies model.path of
                newEnemies ->
                    { model
                        | towers = cooldownTowers delta towers
                        , enemies = killEnemies newEnemies
                        , money = model.money + moneyfromKilledEnemies newEnemies
                        , delta = delta
                    }


update : Float -> Model -> ( Model, Cmd msg )
update delta model =
    ( case model.gameState of
        Running ->
            tick model delta

        Paused ->
            { model | delta = delta }

        Won ->
            { model | delta = delta }

        Lost ->
            { model | delta = delta }
    , Cmd.none
    )
