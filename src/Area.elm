module Area exposing (Area, Field(..), area, fieldSize, fieldToPixel, heightTiles, pixelToField, widthTiles)

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
    10


heightTiles : Int
heightTiles =
    10


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
