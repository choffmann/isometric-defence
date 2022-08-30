module Update.Tick exposing (update)

import Area exposing (Field(..), isOutOfBounds)
import Enemy exposing (Enemy)
import Model exposing (GameState(..), Model)
import Path exposing (Path)
import Point exposing (Point)
import Screen exposing (Screen(..))
import Tower exposing (Tower)
import Ui.Animation as Animation
import Ui.Screens.StartScreen as StartScreen


damageEnemies : Tower -> List Enemy -> ( Tower, List Enemy )
damageEnemies =
    let
        internal : List Enemy -> Tower -> List Enemy -> ( Tower, List Enemy )
        internal acc tower enemyList =
            case enemyList of
                [] ->
                    ( tower
                    , acc
                        |> List.sortBy (\enemy -> enemy.distance)
                    )

                enemy :: hs ->
                    if tower.lastShot > tower.attackSpeed && inRange tower.position tower.attackRadius enemy.position then
                        internal (({ enemy | hp = enemy.hp - tower.damage } :: acc) ++ hs) { tower | lastShot = 0 } []

                    else
                        internal (enemy :: acc) tower hs
    in
    internal []


dealingDamage : List Tower -> List Enemy -> ( List Tower, List Enemy )
dealingDamage =
    let
        rec newTowers oldTowers ( tower, enemies ) =
            internal (tower :: newTowers) oldTowers enemies

        internal : List Tower -> List Tower -> List Enemy -> ( List Tower, List Enemy )
        internal acc towers enemies =
            case towers of
                [] ->
                    ( acc, enemies )

                tower :: hs ->
                    if tower.lastShot > tower.attackSpeed then
                        damageEnemies tower enemies
                            |> rec acc hs

                    else
                        internal (tower :: acc) hs enemies
    in
    internal []


inRange : Point -> Float -> Field -> Bool
inRange point1 radius (Field point2) =
    let
        _ =
            Debug.log "" { point1 = point1, radius = radius, point2 = point2, range = sqrt (pointToQuadrat point1.x point2.x + pointToQuadrat point1.y point2.y) }

        pointToQuadrat first second =
            abs (first - second)
                ^ 2
                |> toFloat
    in
    sqrt (pointToQuadrat point1.x point2.x + pointToQuadrat point1.y point2.y)
        |> (>=) radius


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
                , position =
                    moveAmount enemy.distance enemy.speed
                        |> Path.distanceToPathPoint path
            }
        )


cooldownTowers : Float -> Float -> List Tower -> List Tower
cooldownTowers globalSpeedMulti delta =
    List.map (\tower -> { tower | lastShot = tower.lastShot + delta * globalSpeedMulti * 0.5 })


tick : Model -> Float -> Model
tick model delta =
    let
        moneyFromKilledEnemies =
            List.foldl
                (\enemy money ->
                    if enemy.hp <= 0 then
                        money + enemy.worth

                    else
                        money
                )
                0

        damageFromFinishedEnemies =
            List.foldl (\enemy damage -> damage + enemy.damage) 0

        killEnemies =
            List.filter (\enemy -> enemy.hp > 0)

        filterFinishedEnemiesByOp op =
            let
                internal (Field point) =
                    op point.x 9999
            in
            List.filter
                (\enemy -> internal enemy.position)

        changeModel towers newEnemies =
            { model
                | towers = cooldownTowers model.speedMulti delta towers
                , enemies =
                    newEnemies
                        |> filterFinishedEnemiesByOp (<)
                        |> killEnemies
                , money =
                    newEnemies
                        |> moneyFromKilledEnemies
                        |> (+) model.money
                , hp =
                    model.hp
                        - (newEnemies
                            |> filterFinishedEnemiesByOp (>=)
                            |> damageFromFinishedEnemies
                          )
                , delta = delta
            }

        checkLoose newModel =
            if newModel.hp > 0 then
                newModel

            else
                { newModel | gameState = Lost, screen = LostScreen }

        checkWin newModel =
            case newModel.enemies of
                [] ->
                    { newModel | gameState = Won, screen = WonScreen }

                _ ->
                    newModel

        setState ( towers, enemies ) =
            case model.path of
                Nothing ->
                    model

                Just path ->
                    enemies
                        |> moveEnemies model.speedMulti delta path
                        |> changeModel towers
                        |> checkLoose
                        |> checkWin
    in
    dealingDamage model.towers model.enemies
        |> setState


startScreenAnimation : Model -> Float -> Model
startScreenAnimation model delta =
    case model.animation of
        Nothing ->
            { model | animation = Just { floor = StartScreen.generateFloor }, delta = delta }

        Just animation ->
            { model | animation = Just { floor = Animation.animatedFloor animation.floor delta 0 0 }, delta = delta }


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

        WaitToStart ->
            { model | delta = delta }

        StartScreenAnimation ->
            startScreenAnimation model delta
    , Cmd.none
    )
