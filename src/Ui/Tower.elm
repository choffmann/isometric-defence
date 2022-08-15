module Ui.Tower exposing (availableTowerPlace, towerRadius, towersToCanvas)

import Area exposing (Field(..))
import Canvas exposing (Renderable, Shape)
import Canvas.Settings
import Canvas.Settings.Line
import Color
import List.Nonempty as Nonempty
import Path exposing (Path, PathDirection(..), PathPoint)
import Pixel
import Point exposing (Point)
import Tower exposing (Tower)
import Ui.DrawUtils as DrawUtils


towerRadius : List Tower -> Renderable
towerRadius towers =
    let
        towerPositionToPixel : Point -> Canvas.Point
        towerPositionToPixel point =
            Field point
                |> Area.fieldToPixel
                |> Pixel.pixelToPoint
                |> Point.toCanvasPoint
    in
    towers
        |> List.map (\tower -> Canvas.circle (towerPositionToPixel tower.position) tower.attackRadius)
        |> Canvas.shapes [ Canvas.Settings.stroke (Color.rgb255 0 0 0), Canvas.Settings.Line.lineWidth 2 ]


towersToCanvas : List Tower -> Renderable
towersToCanvas towers =
    towers
        |> List.map
            (\tower ->
                DrawUtils.pointToCanvas tower.position (toFloat Area.fieldSize) (toFloat Area.fieldSize)
            )
        |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 50 255) ]


availableTowerPlace : Maybe Path -> Renderable
availableTowerPlace path =
    let
        drawPoint : PathPoint -> List Shape
        drawPoint point =
            -- Zeichnet einmal komplett um den Pfad eine Fläche, wo ein Turm platziert werden kann
            DrawUtils.pointToCanvas (Point (point.point.x - 1) point.point.y) (toFloat Area.fieldSize) (toFloat Area.fieldSize)
                :: [ DrawUtils.pointToCanvas (Point (point.point.x - 1) (point.point.y - 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                ++ [ DrawUtils.pointToCanvas (Point (point.point.x - 1) (point.point.y + 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                ++ [ DrawUtils.pointToCanvas (Point point.point.x (point.point.y - 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                ++ [ DrawUtils.pointToCanvas (Point (point.point.x + 1) (point.point.y - 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                ++ [ DrawUtils.pointToCanvas (Point (point.point.x + 1) point.point.y) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                ++ [ DrawUtils.pointToCanvas (Point (point.point.x + 1) (point.point.y + 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
                ++ [ DrawUtils.pointToCanvas (Point point.point.x (point.point.y + 1)) (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]

        draw : List PathPoint -> List Shape -> List Shape
        draw drawPath list =
            case drawPath of
                [] ->
                    list

                x :: xs ->
                    draw xs (list ++ drawPoint x)
    in
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 100 255 100) ] (draw (Nonempty.toList justPath) [])
