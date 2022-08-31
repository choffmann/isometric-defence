module Ui.FiredShot exposing (drawShot)

import Area exposing (Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Line as Line
import Color
import Enemy exposing (Enemy)
import GameView exposing (GameView(..))
import Model exposing (FiredShot)
import Point exposing (Point)
import Tower exposing (Tower)
import Ui.DrawUtils as DrawUtils


drawShot : GameView -> List Tower -> List Enemy -> List FiredShot -> List Renderable
drawShot gameView towerList enemyList =
    let
        createVector : ( Canvas.Point, Canvas.Point ) -> ( Float, Float )
        createVector ( ( ex, ey ), ( tx, ty ) ) =
            ( tx - ex, ty - ey )

        createPointFromDistance : Float -> ( Float, Float )
        createPointFromDistance distance =
            let
                length =
                    0.2
            in
            ( distance - length, distance + length )

        calcRatio : Float -> ( Float, Float ) -> ( Float, Float )
        calcRatio range ( dx, dy ) =
            ( dx / range, dy / range )

        addToVector : ( Float, Float ) -> ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) )
        addToVector ( v1, v2 ) ( r1, r2 ) =
            ( ( v1 * r1, v2 * r1 ), ( v1 * r2, v2 * r2 ) )

        addTowerPosition : Canvas.Point -> ( ( Float, Float ), ( Float, Float ) ) -> ( ( Float, Float ), ( Float, Float ) )
        addTowerPosition ( x, y ) ( ( x1, y1 ), ( x2, y2 ) ) =
            ( ( x1 + x, y1 + y )
            , ( x2 + x, y2 + y )
            )

        calcPosition : Float -> Float -> Maybe Enemy -> Maybe Tower -> Maybe ( Canvas.Point, Canvas.Point )
        calcPosition distance range maybeEnemy maybeTower =
            case maybeTower of
                Nothing ->
                    Nothing

                Just tower ->
                    case maybeEnemy of
                        Nothing ->
                            Nothing

                        Just enemy ->
                            Just
                                (createPointFromDistance distance
                                    |> calcRatio range
                                    |> addToVector (createVector ( fieldToPoint tower.position |> DrawUtils.centerPoint gameView, fieldToPoint enemy.position |> DrawUtils.centerPoint gameView ))
                                    |> addTowerPosition (fieldToPoint tower.position |> DrawUtils.centerPoint gameView)
                                )

        fieldToPoint : Field -> Point
        fieldToPoint (Field point) =
            point

        toTopDown : Canvas.Point -> Canvas.Point
        toTopDown ( x, y ) =
            ( x * toFloat Area.fieldSize, y * toFloat Area.fieldSize )

        toIsometric : Canvas.Point -> Canvas.Point
        toIsometric point =
            Area.canvasPointToIsometric Area.isometricMatrix point
                |> Area.isometricOffset
    in
    List.map
        (\shot ->
            case calcPosition shot.distance shot.range (Enemy.findEnemyById shot.enemyId enemyList) (Tower.findTowerById shot.towerId towerList) of
                Nothing ->
                    Canvas.shapes [] []

                Just ( towerPoint, enemyPoint ) ->
                    case gameView of
                        TopDown ->
                            Canvas.shapes [ Settings.stroke (Color.rgb255 0 0 0), Line.lineWidth 4 ]
                                [ Canvas.path (toTopDown towerPoint)
                                    [ Canvas.lineTo (toTopDown enemyPoint) ]
                                ]

                        Isometric ->
                            Canvas.shapes [ Settings.stroke (Color.rgb255 0 0 0), Line.lineWidth 4 ]
                                [ Canvas.path (toIsometric towerPoint)
                                    [ Canvas.lineTo (toIsometric enemyPoint) ]
                                ]
        )
