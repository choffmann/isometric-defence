module Ui.Tower exposing (availableTowerPlace, towersToCanvas)

import Area
import Canvas exposing (Renderable, Shape)
import Canvas.Settings
import Color
import List.Nonempty as Nonempty
import Path exposing (Path, PathDirection(..), PathPoint)
import Point exposing (Point)
import Tower exposing (Tower)
import Ui.DrawUtils as DrawUtils


towersToCanvas : List Tower -> Renderable
towersToCanvas towers =
    towers
        |> List.map
            (\tower ->
                DrawUtils.pointToCanvas tower.position 20 20
            )
        |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 50 255) ]


availableTowerPlace : Maybe Path -> Renderable
availableTowerPlace path =
    let
        draw : PathPoint -> Shape
        draw point =
            case point.direction of
                Up ->
                    DrawUtils.pointToCanvas (Point (point.point.x - 1) point.point.y) (toFloat Area.fieldSize) (toFloat Area.fieldSize)

                -- :: [ DrawUtils.pointToCanvas (Point (point.point.x + 1) point.point.y) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                -- :: [DrawUtils.pointToCanvas (Point point.point.x (point.point.y + 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize)]
                Down ->
                    DrawUtils.pointToCanvas (Point (point.point.x - 1) point.point.y) (toFloat Area.fieldSize) (toFloat Area.fieldSize)

                -- :: [ DrawUtils.pointToCanvas (Point (point.point.x + 1) point.point.y) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                Right ->
                    DrawUtils.pointToCanvas (Point point.point.x (point.point.y - 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize)

        --:: [ DrawUtils.pointToCanvas (Point point.point.x (point.point.y + 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
    in
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 255 50) ] (List.map draw (Nonempty.toList justPath))
