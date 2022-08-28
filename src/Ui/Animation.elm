module Ui.Animation exposing (..)

import Area
import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Ui.DrawUtils as DrawUtils


type alias Animation =
    { floor : List Floor }


type alias Floor =
    { position : Canvas.Point }


drawFloor : Texture -> List Floor -> List Renderable
drawFloor texture floor =
    case floor of
        [] ->
            []

        x :: xs ->
            DrawUtils.placeTileOnCanvas x.position texture :: drawFloor texture xs


animatedFloor : List Floor -> Float -> List Floor
animatedFloor floor delta =
    let
        speed : Float
        speed =
            0.5

        movePoint : Canvas.Point -> Canvas.Point
        movePoint ( x, y ) =
            ( x, y - (delta / 100) )
    in
    case floor of
        [] ->
            []

        x :: xs ->
            { position = movePoint x.position } :: animatedFloor xs delta
