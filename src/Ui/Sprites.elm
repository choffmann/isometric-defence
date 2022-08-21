module Ui.Sprites exposing (..)

import Area
import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Point exposing (Point)
import Ui.DrawUtils as DrawUtils


type alias TowerSelectionSprite =
    { towerCanPlaced : Texture, towerCanNotPlaced : Texture }


type alias Sprites =
    { floor : Texture
    , path : Texture
    , tower :
        { selectTower : TowerSelectionSprite
        , tower1 : Texture
        }
    }


renderFloorSprite : Texture -> List Renderable
renderFloorSprite sprite =
    let
        placeTile : Point -> Renderable
        placeTile { x, y } =
            Canvas.texture [] (DrawUtils.isometricOffset (DrawUtils.toIsometric ( toFloat x, toFloat y ))) sprite

        drawWidth : List Renderable -> Int -> Int -> List Renderable
        drawWidth list i j =
            if j >= Area.widthTiles then
                list

            else
                placeTile (Point i j) :: drawWidth list i (j + 1)

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


renderPathSprite : Texture -> Renderable
renderPathSprite sprite =
    Canvas.texture [] ( 0, 0 ) sprite



--Canvas.texture [] (DrawUtils.isometricOffset (DrawUtils.toIsometric ( 1 * toFloat Area.fieldSize, 1 * toFloat Area.fieldSize ))) sprite
