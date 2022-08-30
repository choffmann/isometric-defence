module Ui.Sprites exposing (renderFloorSprite)

import Area
import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Point exposing (Point)
import Ui.DrawUtils as DrawUtils


renderFloorSprite : Texture -> List Renderable
renderFloorSprite texture =
    let
        drawWidth : List Renderable -> Int -> Int -> List Renderable
        drawWidth list i j =
            if j >= Area.widthTiles then
                list

            else
                DrawUtils.placeIsometricTile (DrawUtils.pointToFloat (Point i j)) texture :: drawWidth list i (j + 1)

        drawHeight : List Renderable -> Int -> List Renderable
        drawHeight list index =
            if index >= Area.heightTiles then
                list

            else
                drawWidth [] index 0 ++ drawHeight list (index + 1)

        draw : List Renderable
        draw =
            drawHeight [] 0
    in
    draw
