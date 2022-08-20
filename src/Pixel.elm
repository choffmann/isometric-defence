module Pixel exposing (Pixel(..), pixelToPoint)

import Point exposing (Point)


type Pixel
    = Pixel Point


pixelToPoint : Pixel -> Point
pixelToPoint (Pixel point) =
    point
