module Ui.DrawUtils exposing (drawCanvasGrid, isometricOffset, pointToCanvas, textOverPoint, toIsometric)

import Area exposing (Area)
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings
import Canvas.Settings.Line
import Canvas.Settings.Text
import Color
import Point exposing (Point)


toIsometric : Canvas.Point -> Canvas.Point
toIsometric ( x, y ) =
    {- ( x * 0.5 + (y * -0.5)
       , x * 0.25 + (y * 0.25)
       )
    -}
    ( x - y, (x + y) / 2 )


isometricOffset : Canvas.Point -> Canvas.Point
isometricOffset ( x, y ) =
    ( x - (toFloat Area.fieldSize / 2) + (toFloat Area.area.width / 2)
    , y
    )


drawCanvasGrid : Renderable
drawCanvasGrid =
    let
        drawLine : Canvas.Point -> Canvas.Point -> List PathSegment
        drawLine fromPoint toPoint =
            [ Canvas.moveTo (isometricOffset (toIsometric fromPoint))
            , Canvas.lineTo (isometricOffset (toIsometric toPoint))
            ]

        --[ Canvas.moveTo fromPoint, Canvas.lineTo toPoint ]
        drawWidth : List PathSegment -> Int -> List PathSegment
        drawWidth list index =
            if index == Area.widthTiles then
                list

            else
                drawLine ( 0, toFloat (index * Area.fieldSize) ) ( toFloat Area.area.width, toFloat (index * Area.fieldSize) )
                    ++ drawWidth list (index + 1)

        drawHeight : List PathSegment -> Int -> List PathSegment
        drawHeight list index =
            if index == Area.heightTiles then
                drawWidth list 0

            else
                drawLine ( toFloat (index * Area.fieldSize), 0 ) ( toFloat (index * Area.fieldSize), toFloat Area.area.height )
                    ++ drawHeight list (index + 1)

        draw : List PathSegment
        draw =
            drawHeight [] 0
    in
    Canvas.shapes [ Canvas.Settings.stroke Color.black, Canvas.Settings.Line.lineWidth 1, Canvas.Settings.Line.lineDash [ 4 ] ] [ Canvas.path ( 0, 0 ) draw ]



-- (draw [] 0)
-- [ Canvas.path ( 0, 0 ) draw ]


textOverPoint : Point -> String -> Renderable
textOverPoint point text =
    Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "arial" } ] (Point.toCanvasPoint point) text


pointToCanvas : Point -> Float -> Float -> Shape
pointToCanvas { x, y } width height =
    Canvas.path (isometricOffset (toIsometric ( toFloat (x * Area.fieldSize), toFloat (y * Area.fieldSize) )))
        [ Canvas.lineTo (isometricOffset (toIsometric ( toFloat (x * Area.fieldSize), toFloat (y * Area.fieldSize) - height )))
        , Canvas.lineTo (isometricOffset (toIsometric ( toFloat (x * Area.fieldSize) + width, toFloat (y * Area.fieldSize) - height )))
        , Canvas.lineTo (isometricOffset (toIsometric ( toFloat (x * Area.fieldSize) + width, toFloat (y * Area.fieldSize) )))
        , Canvas.lineTo (isometricOffset (toIsometric ( toFloat (x * Area.fieldSize) - width, toFloat (y * Area.fieldSize) )))
        ]
