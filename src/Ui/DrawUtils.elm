module Ui.DrawUtils exposing (drawCanvasGrid2d, pointToCanvas, textOverPoint)

import Area exposing (Area)
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings.Line
import Canvas.Settings.Text
import Point exposing (Point)



--( x, y )
{-
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
               [ Canvas.moveTo (isometricOffset (Area.canvasPointToIsometric fromPoint))
               , Canvas.lineTo (isometricOffset (Area.canvasPointToIsometric toPoint))
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
-}
--[ Canvas.path ( 0, 0 ) draw ]


textOverPoint : Point -> String -> Renderable
textOverPoint point text =
    Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "arial" } ] (Point.toCanvasPoint point) text



{- pointToCanvas : Point -> Float -> Float -> Shape
   pointToCanvas { x, y } width height =
       Canvas.path (isometricOffset (Area.canvasPointToIsometric ( toFloat x, toFloat y )))
           [ Canvas.lineTo (isometricOffset (Area.canvasPointToIsometric ( toFloat x, toFloat y - height )))
           , Canvas.lineTo (isometricOffset (Area.canvasPointToIsometric ( toFloat x + width, toFloat y - height )))
           , Canvas.lineTo (isometricOffset (Area.canvasPointToIsometric ( toFloat x + width, toFloat y )))
           , Canvas.lineTo (isometricOffset (Area.canvasPointToIsometric ( toFloat x - width, toFloat y )))
           ]
-}


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
