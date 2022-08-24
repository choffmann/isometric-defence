module Ui.Sprites exposing (Sprites, TowerSelectionSprite, TowerSprites, renderFloorSprite)

import Area
import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Point exposing (Point)
import Ui.DrawUtils as DrawUtils


type alias TowerSelectionSprite =
    { towerCanPlaced : Texture, towerCanNotPlaced : Texture }


type alias TowerSprites =
    { basic : Texture, tower1 : Texture }


type alias Sprites =
    { floor : Texture
    , path : Texture
    , tower :
        { selectTower : TowerSelectionSprite
        , towers : TowerSprites
        }
    , enemy : Texture
    }


renderFloorSprite : Texture -> List Renderable
renderFloorSprite texture =
    let
        drawWidth : List Renderable -> Int -> Int -> List Renderable
        drawWidth list i j =
            if j >= Area.widthTiles then
                list

            else
                DrawUtils.placeTile (Point i j) texture :: drawWidth list i (j + 1)

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



--Canvas.texture [] (DrawUtils.isometricOffset (DrawUtils.toIsometric ( 1 * toFloat Area.fieldSize, 1 * toFloat Area.fieldSize ))) sprite