module Ui.Path exposing (pathToCanvas)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings
import Color
import Path exposing (Path(..))
import Ui.DrawUtils as DrawUtils


pathToCanvas : Maybe Path -> Renderable
pathToCanvas path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just (Last _ justPath) ->
            Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 255 50 50) ] (List.map (\pathPoint -> DrawUtils.pointToCanvas pathPoint.point (toFloat Area.fieldSize) (toFloat Area.fieldSize)) justPath)
