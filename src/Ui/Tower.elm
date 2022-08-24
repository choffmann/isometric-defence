module Ui.Tower exposing (towerArea, towerCanvas, towerRadius, towersToCanvas)

import Area exposing (Area, Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Settings.Line
import Canvas.Settings.Text
import Color
import Pixel
import Point exposing (Point)
import Tower exposing (Tower, Towers(..))
import Ui.DrawUtils as DrawUtils


towerRadius : List Tower -> Renderable
towerRadius towers =
    let
        centerPoint : Point -> Point
        centerPoint { x, y } =
            Point (x + (Area.fieldSize // 2)) (y + (Area.fieldSize // 2))

        towerPositionToPixel : Point -> Canvas.Point
        towerPositionToPixel point =
            Field point
                |> Area.fieldToPixel
                |> Pixel.pixelToPoint
                |> centerPoint
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


demoTowers : List Tower
demoTowers =
    [ Tower.toTower Basic
    , Tower.toTower Tower1
    , Tower.toTower Tower2
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
    Area Area.area.width (towerFieldSize * maxTowerAreaHeight demoTowers)


towersToSelectArea : List Tower -> List Renderable
towersToSelectArea towers =
    let
        currentHeight : Int -> Int
        currentHeight delta =
            floor (toFloat delta / toFloat maxTowerAreaWidth)

        canvasShape : Int -> Int -> Tower -> Renderable
        canvasShape index delta tower =
            Canvas.group []
                [ Canvas.shapes [ Canvas.Settings.fill Color.green, Canvas.Settings.stroke Color.blue ] [ Canvas.rect ( toFloat (index * towerFieldSize), toFloat (currentHeight delta * towerFieldSize) ) (toFloat towerFieldSize) (toFloat towerFieldSize) ]

                -- Display Tower price with an offset of x: +5 and y: -5
                , Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "arial" } ] ( toFloat ((index * towerFieldSize) + 5), toFloat ((currentHeight delta * towerFieldSize) + towerFieldSize - 5) ) (String.fromInt tower.price)
                ]

        draw : Int -> Int -> List Tower -> List Renderable -> List Renderable
        draw index delta towerList list =
            case towerList of
                [] ->
                    list

                x :: xs ->
                    if index >= (maxTowerAreaWidth - 1) then
                        draw 0 (delta + 1) xs (canvasShape index delta x :: list)

                    else
                        draw (index + 1) (delta + 1) xs (canvasShape index delta x :: list)
    in
    draw 0 0 towers []


towerCanvas : List Renderable
towerCanvas =
    [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat towerArea.width) (toFloat towerArea.height) ]
    , DrawUtils.drawCanvasGrid towerArea towerFieldSize
    ]
        ++ towersToSelectArea demoTowers
