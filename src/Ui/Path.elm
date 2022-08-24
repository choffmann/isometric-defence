module Ui.Path exposing (pathToCanvas, renderPathSprite)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Texture exposing (Texture)
import Color
import Path exposing (Path)
import Ui.DrawUtils as DrawUtils


pathToCanvas : Maybe Path -> Renderable
pathToCanvas path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 194 27 29) ] (List.map (\pathPoint -> DrawUtils.pointToCanvas pathPoint.point (toFloat Area.fieldSize) (toFloat Area.fieldSize)) justPath)


renderPathSprite : Maybe Path -> Texture -> List Renderable
renderPathSprite maybePath texture =
    case maybePath of
        Nothing ->
            [ Canvas.shapes [] [] ]

        Just path ->
            List.map
                (\pathPoint ->
                    DrawUtils.placeTile pathPoint.point texture
                )
                path
