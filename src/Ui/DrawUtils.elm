module Ui.DrawUtils exposing (drawCanvasGrid2d, placeTile, pointToCanvas, textOverPoint)

import Area exposing (Area)
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings.Line
import Canvas.Settings.Text
import Canvas.Texture exposing (Texture)
import Point exposing (Point)


textOverPoint : Point -> String -> Renderable
textOverPoint point text =
    Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "arial" } ] (Point.toCanvasPoint point) text


drawCanvasGrid2d : Area -> Int -> Renderable
drawCanvasGrid2d area fieldSize =
    let
        drawLine : Canvas.Point -> Canvas.Point -> List PathSegment
        drawLine fromPoint toPoint =
            [ Canvas.moveTo fromPoint, Canvas.lineTo toPoint ]

        drawWidth : List PathSegment -> Int -> Int -> List PathSegment
        drawWidth list i j =
            if j >= area.width // fieldSize then
                list

            else
                drawLine ( toFloat (i * fieldSize), toFloat (j * fieldSize) ) ( toFloat (i * fieldSize), toFloat area.width )

        drawHeight : List PathSegment -> Int -> List PathSegment
        drawHeight list index =
            if index == area.width then
                list

            else
                drawWidth [] index 0 ++ drawHeight list (index + 1)

        draw : List PathSegment
        draw =
            drawHeight [] 0
    in
    Canvas.shapes [ Canvas.Settings.Line.lineWidth 1, Canvas.Settings.Line.lineDash [ 4 ] ] [ Canvas.path ( 0, 0 ) draw ]


pointToCanvas : Point -> Float -> Float -> Shape
pointToCanvas point width height =
    Canvas.rect ( toFloat (point.x * Area.fieldSize), toFloat (point.y * Area.fieldSize) ) width height


placeTile : Point -> Texture -> Renderable
placeTile { x, y } texture =
    Canvas.texture []
        (Area.canvasPointToIsometric ( toFloat x, toFloat y )
            |> Area.isometricOffset
        )
        texture
