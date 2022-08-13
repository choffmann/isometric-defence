module Ui.Tower exposing (..)

import Canvas exposing (Renderable)
import Canvas.Settings
import Color
import Tower exposing (Tower)
import Ui.DrawUtils as DrawUtils


towersToCanvas : List Tower -> Renderable
towersToCanvas towers =
    towers
        |> List.map
            (\tower ->
                DrawUtils.pointToCanvas tower.position 20 20
            )
        |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 50 255) ]
