module Point exposing (Point, toCanvasPoint)

import Canvas


type alias Point =
    { x : Int
    , y : Int
    }


toCanvasPoint : Point -> Canvas.Point
toCanvasPoint point =
    ( toFloat point.x, toFloat point.y )
