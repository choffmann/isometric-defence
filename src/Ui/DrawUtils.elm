module Ui.DrawUtils exposing (drawCanvasGrid, pointToCanvas)

import Area
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings.Line
import Point exposing (Point)


drawCanvasGrid : Renderable
drawCanvasGrid =
    let
        drawLine : Float -> Float -> Float -> Float -> List PathSegment
        drawLine fromX fromY toX toY =
            [ Canvas.moveTo ( fromX, fromY ), Canvas.lineTo ( toX, toY ) ]

        drawWidth : List PathSegment -> Int -> List PathSegment
        drawWidth list index =
            if index == Area.area.height then
                list

            else
                drawLine (toFloat (index * Area.fieldSize)) 0 (toFloat (index * Area.fieldSize)) (toFloat Area.area.height)
                    |> List.append (drawWidth list (index + 1))

        drawHeight : List PathSegment -> Int -> List PathSegment
        drawHeight list index =
            if index == Area.area.width then
                drawWidth list 0

            else
                drawLine 0 (toFloat (index * Area.fieldSize)) (toFloat Area.area.width) (toFloat (index * Area.fieldSize))
                    |> List.append (drawHeight list (index + 1))

        draw : List PathSegment
        draw =
            drawHeight [] 0
    in
    Canvas.shapes [ Canvas.Settings.Line.lineWidth 1, Canvas.Settings.Line.lineDash [ 4 ] ] [ Canvas.path ( 0, 0 ) draw ]


pointToCanvas : Point -> Float -> Float -> Shape
pointToCanvas point width height =
    Canvas.rect ( toFloat (point.x * Area.fieldSize), toFloat (point.y * Area.fieldSize) ) width height
