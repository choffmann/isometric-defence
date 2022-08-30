module Ui.Enemy exposing (enemiesToCanvas, renderEnemyIso)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Texture exposing (Texture)
import Color
import Enemy exposing (Enemies(..), Enemy)
import List.Extra as List
import Path exposing (Path)
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Sprite exposing (EnemyTexture)
import Ui.DrawUtils as DrawUtils


enemiesToCanvas : List Enemy -> Maybe Path -> List Renderable
enemiesToCanvas enemies maybePath =
    case maybePath of
        Nothing ->
            [ Canvas.shapes [] [] ]

        Just justPath ->
            enemies
                |> List.map
                    (\enemy ->
                        Path.distanceToPixel justPath enemy.distance
                            |> Maybe.map
                                (\(Pixel point) ->
                                    Canvas.group []
                                        [ Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 255 50) ] [ Canvas.rect ( toFloat point.x - 10, toFloat point.y - 10 ) 20 20 ]
                                        , DrawUtils.drawTextOverPoint (Point point.x (point.y - ((Area.fieldSize // 2) + 3))) (String.fromInt enemy.hp)
                                        ]
                                )
                    )
                |> List.removeNothing


renderEnemyIso : List Enemy -> Maybe Path -> EnemyTexture -> List Renderable
renderEnemyIso enemies maybePath texture =
    case maybePath of
        Nothing ->
            [ Canvas.shapes [] [] ]

        Just path ->
            enemies
                |> List.map
                    (\enemy ->
                        Path.distanceToPixel path enemy.distance
                            |> Maybe.map
                                (\(Pixel point) ->
                                    -- DrawUtils.placeTileOnCanvas ( (toFloat point.x / toFloat Area.fieldSize) - 1, (toFloat point.y / toFloat Area.fieldSize) - 1 ) (enemyTexture enemy texture) Area.isometricMatrix
                                    DrawUtils.placeTileOnCanvas ( toFloat (point.x // Area.fieldSize) - 1, toFloat (point.y // Area.fieldSize) - 1 ) (enemyTexture enemy texture) Area.isometricMatrix
                                )
                    )
                |> List.removeNothing


enemyTexture : Enemy -> EnemyTexture -> Texture
enemyTexture enemy texture =
    case enemy.enemyType of
        CardBoardBox ->
            texture.cardBoardBox

        WoodenBox ->
            texture.woodBox

        RedBox ->
            texture.redBox

        YellowBox ->
            texture.yellowBox

        BlueBox ->
            texture.blueBox

        MetalBox ->
            texture.metalBox

        Palette ->
            texture.metalBox
