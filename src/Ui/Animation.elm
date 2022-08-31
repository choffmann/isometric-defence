module Ui.Animation exposing (Animation, Floor, animatedFloor, drawFloor)

import Area exposing (IsometricMatrix)
import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Ui.DrawUtils as DrawUtils


type alias Animation =
    { floor : List Floor }


type alias Floor =
    { position : Canvas.Point
    , matrix : IsometricMatrix
    , elapsedTime : Float
    }


drawFloor : Texture -> List Floor -> List Renderable
drawFloor texture floor =
    case floor of
        [] ->
            []

        x :: xs ->
            DrawUtils.placeIsometricTileWithMatrix x.position x.matrix texture :: drawFloor texture xs


animatedFloor : Float -> Float -> Float -> List Floor -> List Floor
animatedFloor delta offsetY1 offsetY2 floor =
    let
        updateMatrix : Float -> IsometricMatrix -> IsometricMatrix
        updateMatrix time matrix =
            let
                speed : Float
                speed =
                    0.002

                amplitude : Float
                amplitude =
                    0.15
            in
            { y1 = amplitude * (matrix.y1 + sin (time * speed + offsetY1))
            , y2 = amplitude * (matrix.y2 + sin (time * speed + offsetY2))
            , x1 = 0.9
            , x2 = -0.9
            }
    in
    case floor of
        [] ->
            []

        x :: xs ->
            { x
                | matrix = updateMatrix x.elapsedTime x.matrix
                , elapsedTime = x.elapsedTime + delta
            }
                :: animatedFloor delta (offsetY1 + 0.008) (offsetY2 + 0.02) xs
