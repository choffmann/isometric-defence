module Area exposing (Area, Field(..), area, fieldSize, pixelToField)

import Pixel exposing (Pixel(..))
import Point exposing (Point)


fieldSize : Int
fieldSize =
    30


type alias Area =
    { width : Int
    , height : Int
    }


area : Area
area =
    Area (20 * fieldSize) (15 * fieldSize)


type Field
    = Field Point


pixelToField : Pixel -> Field
pixelToField (Pixel { x, y }) =
    Field { x = x // fieldSize, y = y // fieldSize }
