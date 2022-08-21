module Area exposing (Area, Field(..), area, canvasPointToIsometric, fieldSize, fieldToPixel, heightTiles, isometricOffset, isometricToPoint, pixelToField, pixelToFieldIso, widthTiles)

import Pixel exposing (Pixel(..))
import Point exposing (Point)


fieldSize : Int
fieldSize =
    32


type alias Area =
    { width : Int
    , height : Int
    }


widthTiles : Int
widthTiles =
    20


heightTiles : Int
heightTiles =
    20


area : Area
area =
    Area (widthTiles * fieldSize) (heightTiles * fieldSize)


type Field
    = Field Point


pixelToFieldIso : Pixel -> Field
pixelToFieldIso (Pixel point) =
    Field (isometricToPoint point)


pixelToField : Pixel -> Field
pixelToField (Pixel { x, y }) =
    Field { x = min (x // fieldSize) (widthTiles - 1), y = min (y // fieldSize) (heightTiles - 1) }


fieldToPixel : Field -> Pixel
fieldToPixel (Field { x, y }) =
    Pixel { x = x * fieldSize, y = y * fieldSize }



-- https://gist.github.com/jordwest/8a12196436ebcf8df98a2745251915b5


type alias IsometricMatrix =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


matrix : IsometricMatrix
matrix =
    { x1 = 1
    , y1 = 0.5
    , x2 = -1
    , y2 = 0.5
    }


canvasPointToIsometric : ( Float, Float ) -> ( Float, Float )
canvasPointToIsometric ( x, y ) =
    ( x * matrix.x1 * 0.5 * toFloat fieldSize + y * matrix.x2 * 0.5 * toFloat fieldSize
    , x * matrix.y1 * 0.5 * toFloat fieldSize + y * matrix.y2 * 0.5 * toFloat fieldSize
    )


isometricOffset : ( Float, Float ) -> ( Float, Float )
isometricOffset ( x, y ) =
    ( x - (toFloat fieldSize / 2) + toFloat area.width / 2
    , y + (toFloat fieldSize / 2)
    )


isometricToPoint : Point -> Point
isometricToPoint { x, y } =
    let
        invertMatrix : Float -> Float -> Float -> Float -> IsometricMatrix
        invertMatrix a b c d =
            let
                det : Float
                det =
                    1 / (a * d - b * c)
            in
            { x1 = det * d
            , y1 = det * -b
            , x2 = det * -c
            , y2 = det * a
            }

        inv : IsometricMatrix
        inv =
            invertMatrix
                (matrix.x1 * 0.5 * toFloat fieldSize)
                (matrix.x2 * 0.5 * toFloat fieldSize)
                (matrix.y1 * 0.5 * toFloat fieldSize)
                (matrix.y2 * 0.5 * toFloat fieldSize)

        withOffset : Point -> Point
        withOffset point =
            let
                backToPoint : ( Float, Float ) -> Point
                backToPoint ( dx, dy ) =
                    Point (floor dx) (floor dy)
            in
            isometricOffset ( toFloat point.x, toFloat point.y )
                |> backToPoint
    in
    Point (floor (toFloat x * inv.x1 + toFloat y * inv.x2)) (floor (toFloat x * inv.y1 + toFloat y * inv.y2))
