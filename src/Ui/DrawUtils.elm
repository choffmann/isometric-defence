module Ui.DrawUtils exposing (centerField, centerPixel, drawCanvasGrid2D, drawTextOverPoint, fieldToCanvas, placeIsometricTile, placeIsometricTileWithMatrix, placeTopDownTile, pointToCanvas, pointToFloat)

import Area exposing (Field(..), IsometricMatrix, Pixel(..))
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings as Settings
import Canvas.Settings.Line
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import GameView exposing (GameView(..))
import Point exposing (Point)


drawCanvasGrid2D : Renderable
drawCanvasGrid2D =
    let
        drawLine : Float -> Float -> Float -> Float -> ( PathSegment, PathSegment )
        drawLine fromX fromY toX toY =
            ( Canvas.moveTo ( fromX, fromY ), Canvas.lineTo ( toX, toY ) )

        drawWidth : Int -> List PathSegment -> List PathSegment
        drawWidth index acc =
            if index == Area.area.height then
                acc

            else
                drawLine (toFloat (index * Area.fieldSize)) 0 (toFloat (index * Area.fieldSize)) (toFloat Area.area.height)
                    |> (\( p1, p2 ) -> p1 :: p2 :: acc)
                    |> drawWidth (index + 1)

        drawHeight : Int -> List PathSegment -> List PathSegment
        drawHeight index acc =
            if index == Area.area.width then
                drawWidth 0 acc

            else
                drawLine 0 (toFloat (index * Area.fieldSize)) (toFloat Area.area.width) (toFloat (index * Area.fieldSize))
                    |> (\( p1, p2 ) -> p1 :: p2 :: acc)
                    |> drawHeight (index + 1)

        draw : List PathSegment
        draw =
            drawHeight 0 []
    in
    Canvas.shapes [ Canvas.Settings.Line.lineWidth 1, Canvas.Settings.Line.lineDash [ 4 ] ] [ Canvas.path ( 0, 0 ) draw ]


centerField : GameView -> Field -> Canvas.Point
centerField gameView (Field { x, y }) =
    case gameView of
        Isometric ->
            ( toFloat x + 0.75, toFloat y - 0.25 )

        TopDown ->
            ( toFloat x + 0.5, toFloat y + 0.5 )


centerPixel : Pixel -> Pixel
centerPixel (Pixel { x, y }) =
    Point (x + (Area.fieldSize // 2)) (y + (Area.fieldSize // 2))
        |> Pixel


pointToCanvas : Field -> Float -> Float -> Shape
pointToCanvas (Field { x, y }) =
    Canvas.rect ( toFloat (x * Area.fieldSize), toFloat (y * Area.fieldSize) )


placeTopDownTile : Canvas.Point -> Texture -> Renderable
placeTopDownTile =
    Canvas.texture []


placeIsometricTile : Canvas.Point -> Texture -> Renderable
placeIsometricTile point =
    Canvas.texture []
        (Area.canvasPointToIsometric Area.isometricMatrix point
            |> Area.isometricOffset
        )


placeIsometricTileWithMatrix : Canvas.Point -> IsometricMatrix -> Texture -> Renderable
placeIsometricTileWithMatrix point matrix =
    Canvas.texture []
        (Area.canvasPointToIsometric matrix point
            |> Area.isometricOffset
        )


fieldToCanvas : Field -> Canvas.Point
fieldToCanvas (Field { x, y }) =
    ( toFloat (x * Area.fieldSize), toFloat (y * Area.fieldSize) )


pointToFloat : Point -> Canvas.Point
pointToFloat { x, y } =
    ( toFloat x, toFloat y )


drawTextOverPoint : Canvas.Point -> String -> Renderable
drawTextOverPoint =
    Canvas.text [ Text.font { size = 12, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle, Settings.fill Color.gray ]
