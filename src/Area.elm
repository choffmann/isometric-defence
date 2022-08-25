module Area exposing (Area, Field(..), area, canvasPointToIsometric, fieldSize, fieldToPixel, heightTiles, isOutOfBounds, isometricOffset, isometricPixelToField, pixelToField, widthTiles)

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


pixelToField : Pixel -> Field
pixelToField (Pixel { x, y }) =
    Field { x = min (x // fieldSize) (widthTiles - 1), y = min (y // fieldSize) (heightTiles - 1) }


fieldToPixel : Field -> Pixel
fieldToPixel (Field { x, y }) =
    Pixel { x = x * fieldSize, y = y * fieldSize }


isOutOfBounds : Maybe Field -> Maybe Field
isOutOfBounds point =
    point
        |> Maybe.andThen
            (\(Field { x, y }) ->
                if x < 0 || x > widthTiles - 1 || y < 0 || y > heightTiles - 1 then
                    Nothing

                else
                    Just (Field (Point x y))
            )



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
    , y + (toFloat area.height / 4)
    )


isometricPixelToField : Pixel -> Field
isometricPixelToField (Pixel { x, y }) =
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
                (matrix.y1 * 0.5 * toFloat fieldSize)
                (matrix.x2 * 0.5 * toFloat fieldSize)
                (matrix.y2 * 0.5 * toFloat fieldSize)

        offset : Point -> Point
        offset point =
            { x = point.x - area.width // 2, y = point.y - floor (toFloat area.height / 4) }

        --+ area.height // 4 }
        calc : Point -> Point
        calc point =
            Point
                (floor (toFloat point.x * inv.x1 + toFloat point.y * inv.x2))
                (floor (toFloat point.x * inv.y1 + toFloat point.y * inv.y2))
    in
    Field
        (Point x y
            |> offset
            |> calc
        )
