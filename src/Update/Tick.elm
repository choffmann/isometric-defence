module Update.Tick exposing (update)

import Area exposing (Field(..))
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


inRange : Point -> Float -> Field -> Bool
inRange point1 radius (Field point2) =
    let
        pointToQuadrat first second =
            abs (first - second)
                ^ 2
                |> toFloat
    in
    sqrt (pointToQuadrat point1.x point2.x + pointToQuadrat point1.y point2.y)
        |> (>) radius


moveEnemies : Float -> Float -> Path -> List Enemy -> List Enemy
moveEnemies globalSpeedMulti delta path =
    let
        moveAmount distance speed =
            distance + (speed * 0.025 * delta * globalSpeedMulti)
    in
    List.map
        (\enemy ->
            { enemy
                | distance = moveAmount enemy.distance enemy.speed
                , position = moveAmount enemy.distance enemy.speed |> distanceToPathPoint path
            }
        )


cooldownTowers : Float -> Float -> List Tower -> List Tower
cooldownTowers globalSpeedMulti delta =
    List.map (\tower -> { tower | lastShot = tower.lastShot + delta * globalSpeedMulti * 0.5 })


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
            case model.path of
                Nothing ->
                    Debug.todo "Path is not greatet yet"

                Just path ->
                    case enemies |> moveEnemies model.speedMulti delta path of
                        newEnemies ->
                            { model
                                | towers = cooldownTowers model.speedMulti delta towers
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

        GeneratePath ->
            { model | delta = delta }
    , Cmd.none
    )
