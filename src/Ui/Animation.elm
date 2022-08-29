module Ui.Animation exposing (..)

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
            DrawUtils.placeTileOnCanvas x.position texture x.matrix :: drawFloor texture xs


animatedFloor : List Floor -> Float -> List Floor
animatedFloor floor delta =
    let
        updateMatrix : Float -> IsometricMatrix -> IsometricMatrix
        updateMatrix time matrix =
            let
                speed : Float
                speed =
                    0.005

                amplitude : Float
                amplitude =
                    0.25
            in
            { matrix
                | y1 = amplitude * (matrix.y1 + sin (time * speed)) --+ 0.1
                , y2 = amplitude * (matrix.y2 + sin (time * speed)) --+ 0.1
            }

        updatePoint : Float -> Canvas.Point -> Canvas.Point
        updatePoint time ( x, y ) =
            let
                speed : Float
                speed =
                    0.01

                amplitude : Float
                amplitude =
                    1
            in
            ( x + sin (time * speed) - 0.5
            , y + sin (time * speed) - 0.5
            )
    in
    case floor of
        [] ->
            []

        x :: xs ->
            {- { position = updatePoint x.elapsedTime x.position
               , elapsedTime = x.elapsedTime + delta
               , matrix = Area.isometricMatrix
               }
                   :: animatedFloor xs delta
            -}
            { x
                | matrix = updateMatrix x.elapsedTime x.matrix
                , elapsedTime = x.elapsedTime + delta
            }
                :: animatedFloor xs delta
