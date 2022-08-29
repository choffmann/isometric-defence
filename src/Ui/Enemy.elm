module Ui.Enemy exposing (enemiesToCanvas, renderEnemyIso)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Texture exposing (Texture)
import Color
import Enemy exposing (Enemy)
import List.Extra as List
import Path exposing (Path)
import Pixel exposing (Pixel(..))
import Ui.DrawUtils as DrawUtils


enemiesToCanvas : List Enemy -> Maybe Path -> Renderable
enemiesToCanvas enemies path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            enemies
                |> List.map
                    (\enemy ->
                        Path.distanceToPixel justPath enemy.distance
                            |> Maybe.map
                                (\(Pixel point) ->
                                    Canvas.rect ( toFloat point.x - 10, toFloat point.y - 10 ) 20 20
                                )
                    )
                |> List.removeNothing
                |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 255 50) ]



-- TODO: Not working


renderEnemyIso : List Enemy -> Maybe Path -> Texture -> List Renderable
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
                                    DrawUtils.placeTileOnCanvas ( (toFloat point.x / toFloat Area.fieldSize) - 1, (toFloat point.y / toFloat Area.fieldSize) - 1 ) texture Area.isometricMatrix
                                )
                    )
                |> List.removeNothing
