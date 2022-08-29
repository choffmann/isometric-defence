module Ui.DrawUtils exposing (drawCanvasGrid2d, drawTextOverPoint, fieldToCanvas, placeTile, placeTileOnCanvas, pointToCanvas, pointToFloat)

import Area exposing (Area, IsometricMatrix)
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings as Settings
import Canvas.Settings.Line
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import Point exposing (Point)


drawCanvasGrid2d : Area -> Int -> Renderable
drawCanvasGrid2d area fieldSize =
    let
        drawLine : Float -> Float -> Float -> Float -> List PathSegment
        drawLine fromX fromY toX toY =
            [ Canvas.moveTo ( fromX, fromY ), Canvas.lineTo ( toX, toY ) ]

        drawWidth : List PathSegment -> Int -> List PathSegment
        drawWidth list index =
            if index == area.height then
                list

            else
                drawLine (toFloat (index * fieldSize)) 0 (toFloat (index * fieldSize)) (toFloat area.height)
                    |> List.append (drawWidth list (index + 1))

        drawHeight : List PathSegment -> Int -> List PathSegment
        drawHeight list index =
            if index == area.width then
                drawWidth list 0

            else
                drawLine 0 (toFloat (index * fieldSize)) (toFloat area.width) (toFloat (index * fieldSize))
                    |> List.append (drawHeight list (index + 1))

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
        (Area.canvasPointToIsometric Area.isometricMatrix ( toFloat x, toFloat y )
            |> Area.isometricOffset
        )
        texture


placeTileOnCanvas : Canvas.Point -> Texture -> IsometricMatrix -> Renderable
placeTileOnCanvas point texture matrix =
    Canvas.texture []
        (Area.canvasPointToIsometric matrix point
            |> Area.isometricOffset
        )
        texture


fieldToCanvas : Point -> Canvas.Point
fieldToCanvas { x, y } =
    ( toFloat (x * Area.fieldSize), toFloat (y * Area.fieldSize) )


pointToFloat : Point -> Canvas.Point
pointToFloat { x, y } =
    ( toFloat x, toFloat y )


drawTextOverPoint : Point -> String -> Renderable
drawTextOverPoint point text =
    Canvas.text [ Text.font { size = 12, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle, Settings.fill Color.gray ] (pointToFloat point) text
