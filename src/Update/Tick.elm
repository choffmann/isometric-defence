module Update.Tick exposing (update)

import Area exposing (Field(..))
import Enemy exposing (Enemy)
import Model exposing (FiredShot, GameState(..), Model)
import Path exposing (Path)
import Round exposing (Round(..))
import Screen exposing (Screen(..))
import Tower exposing (Tower)
import Ui.Animation as Animation
import Ui.Screens.StartScreen as StartScreen


shootEnemy : Tower -> List Enemy -> ( Tower, Maybe FiredShot )
shootEnemy tower enemies =
    case enemies of
        [] ->
            ( tower, Nothing )

        enemy :: hs ->
            if tower.lastShot > tower.attackSpeed && inRange tower.position tower.attackRadius enemy.position then
                ( { tower | lastShot = 0 }, Just (FiredShot enemy.id tower.id (range tower.position enemy.position) 0) )

            else
                shootEnemy tower hs


shoot : List Tower -> List Enemy -> ( List Tower, List FiredShot )
shoot =
    let
        internal : List Tower -> List FiredShot -> List Tower -> List Enemy -> ( List Tower, List FiredShot )
        internal acc acc2 towers enemies =
            case towers of
                [] ->
                    ( acc, acc2 )

                tower :: hs ->
                    if tower.lastShot > tower.attackSpeed then
                        case shootEnemy tower enemies of
                            ( newTower, Nothing ) ->
                                internal (newTower :: acc) acc2 hs enemies

                            ( newTower, Just firedshot ) ->
                                internal (newTower :: acc) (firedshot :: acc2) hs enemies

                    else
                        internal (tower :: acc) acc2 hs enemies
    in
    internal [] []


damageEnemies : FiredShot -> List Tower -> List Enemy -> List Enemy
damageEnemies shot towers =
    let
        internal acc enemies =
            case enemies of
                [] ->
                    acc

                h :: hs ->
                    if h.id == shot.enemyId then
                        case Tower.findTowerById shot.towerId towers of
                            Nothing ->
                                internal (h :: acc ++ hs) []

                            Just tower ->
                                internal ({ h | hp = h.hp - tower.damage } :: acc ++ hs) []

                    else
                        internal (h :: acc) hs
    in
    internal []


dealingDamage : List FiredShot -> List Tower -> List Enemy -> List Enemy
dealingDamage shotsFired towers enemies =
    case shotsFired of
        [] ->
            enemies

        h :: hs ->
            dealingDamage hs towers (damageEnemies h towers enemies)


range : Field -> Field -> Float
range (Field point1) (Field point2) =
    let
        pointToQuadrat first second =
            abs (first - second)
                ^ 2
                |> toFloat
    in
    sqrt (pointToQuadrat point1.x point2.x + pointToQuadrat point1.y point2.y)


inRange : Field -> Float -> Field -> Bool
inRange field1 radius field2 =
    range field1 field2
        |> (>=) radius


moveEnemies : Float -> Float -> Path -> List Enemy -> List Enemy
moveEnemies globalSpeedMulti delta path enemies =
    let
        moveAmount distance speed =
            distance + (speed * 0.025 * delta * globalSpeedMulti)
    in
    enemies
        |> List.map
            (\enemy ->
                { enemy
                    | distance = moveAmount enemy.distance enemy.speed
                    , position =
                        moveAmount enemy.distance enemy.speed
                            |> Path.distanceToPathPoint path
                }
            )
        |> List.sortWith
            (\e1 e2 ->
                if e1.distance > e2.distance then
                    LT

                else if e1.distance < e2.distance then
                    GT

                else
                    EQ
            )


cooldownTowers : Float -> Float -> List Tower -> List Tower
cooldownTowers globalSpeedMulti delta =
    List.map (\tower -> { tower | lastShot = tower.lastShot + delta * globalSpeedMulti })


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

        changeModel towers firedshots newEnemies =
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
                , shotsFired =
                    firedshots
                        |> List.map (\shot -> { shot | distance = shot.distance + (delta * model.speedMulti / 50) })
                , delta = delta
            }

        checkLoose newModel =
            if newModel.hp > 0 then
                newModel

            else
                { newModel | gameState = Lost, screen = LostScreen }

        checkNextRound newModel =
            case newModel.enemies of
                [] ->
                    case model.round of
                        Round1 ->
                            { newModel
                                | enemies = Enemy.round2
                                , round = Round2
                                , gameState = WaitToStart
                                , money = model.money + 200
                                , shotsFired = []
                            }

                        Round2 ->
                            { newModel
                                | enemies = Enemy.round3
                                , round = Round3
                                , gameState = WaitToStart
                                , money = model.money + 300
                                , shotsFired = []
                            }

                        Round3 ->
                            { newModel
                                | enemies = Enemy.round4
                                , round = Round4
                                , gameState = WaitToStart
                                , money = model.money + 400
                                , shotsFired = []
                            }

                        Round4 ->
                            { newModel
                                | enemies = Enemy.round5
                                , round = Round5
                                , gameState = WaitToStart
                                , money = model.money + 500
                                , shotsFired = []
                            }

                        Round5 ->
                            { newModel
                                | enemies = Enemy.round6
                                , round = Round6
                                , gameState = WaitToStart
                                , money = model.money + 600
                                , shotsFired = []
                            }

                        Round6 ->
                            { newModel
                                | enemies = Enemy.round7
                                , round = Round7
                                , gameState = WaitToStart
                                , money = model.money + 700
                                , shotsFired = []
                            }

                        Round7 ->
                            { newModel
                                | gameState = Won
                                , screen = WonScreen
                            }

                _ ->
                    newModel

        setState ( towers, firedshots ) =
            case model.path of
                Nothing ->
                    model

                Just path ->
                    model.enemies
                        |> moveEnemies model.speedMulti delta path
                        |> dealingDamage
                            ((firedshots ++ model.shotsFired)
                                |> List.filter (\shot -> shot.distance >= shot.range)
                            )
                            model.towers
                        |> changeModel towers
                            ((firedshots ++ model.shotsFired)
                                |> List.filter (\shot -> shot.distance < shot.range)
                            )
                        |> checkLoose
                        |> checkNextRound
    in
    shoot model.towers model.enemies
        |> setState


startScreenAnimation : Model -> Float -> Model
startScreenAnimation model delta =
    case model.animation of
        Nothing ->
            { model | animation = Just { floor = StartScreen.generateFloor }, delta = delta }

        Just animation ->
            { model | animation = Just { floor = Animation.animatedFloor delta 0 0 animation.floor }, delta = delta }


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
