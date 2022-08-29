module Point exposing (Point, toCanvasPoint)

import Canvas


type alias Point =
    { x : Int
    , y : Int
    }



-- TODO: wo anders hin, macht warscheinlich mehr sinn, wenn man hier direkt den Punkt zur Canvas Position bekommt
-- z.B.: {x=3, x=5} => (toFloat x * Area.fieldSize, toFloat y * fieldSize)


toCanvasPoint : Point -> Canvas.Point
toCanvasPoint point =
    ( toFloat point.x, toFloat point.y )
