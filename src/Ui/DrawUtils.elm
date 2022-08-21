module Ui.DrawUtils exposing (drawCanvasGrid, drawCanvasGrid2d, isometricOffset, pointToCanvas, textOverPoint, toIsometric)

import Area exposing (Area)
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings
import Canvas.Settings.Line
import Canvas.Settings.Text
import Color
import Point exposing (Point)


toIsometric : Canvas.Point -> Canvas.Point
toIsometric ( x, y ) =
    ( (x * 0.5 * toFloat Area.fieldSize) + (y * -0.5 * toFloat Area.fieldSize)
    , (x * 0.25 * toFloat Area.fieldSize) + (y * 0.25 * toFloat Area.fieldSize)
    )



{- ( x - y, (x + y) / 2 ) -}


isometricOffset : Canvas.Point -> Canvas.Point
isometricOffset ( x, y ) =
    -- TODO: Canvas anpassen
    -- Erstmal, damit es passt, entweder:
    --  toIsometric: ( x - y, (x + y) / 2 )
    --  isometricOffset: ( x - (toFloat Area.fieldSize / 2) + toFloat Area.area.width - toFloat Area.fieldSize
    --  view: Area.width * 2
    -- oder:
    --  toIsometric: ( (x * 0.5) + (y * -0.5), (x * 0.25) + (y * 0.25))
    --  isometricOffset: ( x - (toFloat Area.fieldSize / 2) + toFloat Area.area.width / 2
    --  view: Area.height // 2
    ( x - (toFloat Area.fieldSize / 2) + toFloat Area.area.width / 2
    , y + (toFloat Area.fieldSize / 2)
    )



--( x, y )


drawCanvasGrid2d : Renderable
drawCanvasGrid2d =
    let
        drawLine : Canvas.Point -> Canvas.Point -> List PathSegment
        drawLine fromPoint toPoint =
            [ Canvas.moveTo fromPoint, Canvas.lineTo toPoint ]

        drawWidth : List PathSegment -> Int -> List PathSegment
        drawWidth list index =
            if index == Area.widthTiles then
                list

            else
                drawLine ( toFloat (index * Area.fieldSize), 0 ) ( toFloat (index * Area.fieldSize), toFloat Area.area.height )
                    |> List.append (drawWidth list (index + 1))

        drawHeight : List PathSegment -> Int -> List PathSegment
        drawHeight list index =
            if index == Area.heightTiles then
                drawWidth list 0

            else
                drawLine ( 0, toFloat (index * Area.fieldSize) ) ( toFloat Area.area.width, toFloat (index * Area.fieldSize) )
                    |> List.append (drawHeight list (index + 1))

        draw : List PathSegment
        draw =
            drawHeight [] 0
    in
    Canvas.shapes [ Canvas.Settings.stroke Color.black, Canvas.Settings.Line.lineWidth 1 ] [ Canvas.path ( 0, 0 ) draw ]


drawCanvasGrid : Renderable
drawCanvasGrid =
    let
        drawLine : Canvas.Point -> Canvas.Point -> List PathSegment
        drawLine fromPoint toPoint =
            [ Canvas.moveTo (isometricOffset (toIsometric fromPoint))
            , Canvas.lineTo (isometricOffset (toIsometric toPoint))
            ]

        drawPoint : Point -> Shape
        drawPoint point =
            pointToCanvas point (toFloat Area.fieldSize) (toFloat Area.fieldSize)

        drawWidth : List Shape -> Int -> List Shape
        drawWidth list index =
            if index == Area.widthTiles then
                list

            else
                drawPoint (Point index 0)
                    :: drawWidth list (index + 1)

        drawHeight : List Shape -> Int -> List Shape
        drawHeight list index =
            if index == Area.heightTiles then
                drawWidth list 0

            else
                drawPoint (Point 0 index)
                    :: drawHeight list (index + 1)

        draw : List Shape
        draw =
            drawHeight [] 0
    in
    Canvas.shapes [ Canvas.Settings.stroke Color.black, Canvas.Settings.Line.lineWidth 1, Canvas.Settings.Line.lineDash [ 4 ] ] draw



--[ Canvas.path ( 0, 0 ) draw ]


textOverPoint : Point -> String -> Renderable
textOverPoint point text =
    Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "arial" } ] (Point.toCanvasPoint point) text


pointToCanvas : Point -> Float -> Float -> Shape
pointToCanvas { x, y } width height =
    Canvas.path (isometricOffset (toIsometric ( toFloat x, toFloat y )))
        [ Canvas.lineTo (isometricOffset (toIsometric ( toFloat x, toFloat y - height )))
        , Canvas.lineTo (isometricOffset (toIsometric ( toFloat x + width, toFloat y - height )))
        , Canvas.lineTo (isometricOffset (toIsometric ( toFloat x + width, toFloat y )))
        , Canvas.lineTo (isometricOffset (toIsometric ( toFloat x - width, toFloat y )))
        ]
