module Ui.Tower exposing (availableTowerPlace, towerArea, towerCanvas, towerFieldSize, towerRadius, towersToCanvas)

import Area exposing (Area, Field(..))
import Canvas exposing (Renderable, Shape)
import Canvas.Settings
import Canvas.Settings.Line
import Canvas.Settings.Text
import Color
import List.Nonempty as Nonempty
import Path exposing (Path, PathDirection(..), PathPoint)
import Pixel
import Point exposing (Point)
import Tower exposing (Tower, Towers(..))
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


availableTowers : List Tower
availableTowers =
    [ Tower.toTower Basic
    , Tower.toTower Tower1
    , Tower.toTower Tower2
    , Tower.toTower Tower3
    , Tower.toTower Tower1
    , Tower.toTower Tower2
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    , Tower.toTower Tower3
    ]


towerFieldSizeFactor : Int
towerFieldSizeFactor =
    2


towerFieldSize : Int
towerFieldSize =
    Area.fieldSize * towerFieldSizeFactor


maxTowerAreaWidth : Int
maxTowerAreaWidth =
    (Area.area.width // Area.fieldSize) // towerFieldSizeFactor


maxTowerAreaHeight : List Tower -> Int
maxTowerAreaHeight towers =
    ceiling (toFloat (List.length towers) / toFloat maxTowerAreaWidth)


towerArea : Area
towerArea =
    Area Area.area.width (towerFieldSize * maxTowerAreaHeight availableTowers)


towerToSelectArea : List Tower -> List Renderable
towerToSelectArea towers =
    let
        currentHeight : Int -> Int
        currentHeight index =
            floor (toFloat index / toFloat maxTowerAreaWidth)

        currentWidth : Int -> Int
        currentWidth index =
            if index >= maxTowerAreaWidth then
                index - maxTowerAreaWidth

            else
                index
    in
    List.indexedMap
        (\index _ ->
            Canvas.shapes [ Canvas.Settings.fill Color.green, Canvas.Settings.stroke Color.blue ] [ Canvas.rect ( toFloat (currentWidth index * towerFieldSize), toFloat (currentHeight index * towerFieldSize) ) (toFloat towerFieldSize) (toFloat towerFieldSize) ]
        )
        towers


towerCanvas : List Renderable
towerCanvas =
    [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat towerArea.width) (toFloat towerArea.height) ]
    , DrawUtils.drawCanvasGrid towerArea towerFieldSize
    ]
        ++ towerToSelectArea availableTowers



-- ++ List.map (\tower -> Canvas.text [ Canvas.Settings.Text.font { size = 10, family = "serif" }, Canvas.Settings.Text.align Canvas.Settings.Text.Center ] (Point.toCanvasPoint tower.position) "Hello world") availableTowers
