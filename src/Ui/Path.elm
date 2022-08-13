module Ui.Path exposing (..)

import Area
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings
import Color
import List.Nonempty as Nonempty
import Path exposing (Path)
import Ui.DrawUtils as DrawUtils


pathToCanvas : Maybe Path -> Renderable
pathToCanvas path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 255 50 50) ] (List.map (\pathPoint -> DrawUtils.pointToCanvas pathPoint.point (toFloat Area.fieldSize) (toFloat Area.fieldSize)) (Nonempty.toList justPath))
