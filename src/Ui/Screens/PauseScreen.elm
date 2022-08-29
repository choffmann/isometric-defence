module Ui.Screens.PauseScreen exposing (..)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Ui.DrawUtils as DrawUtils


text : Renderable
text =
    Canvas.text [ Text.font { size = Area.fieldSize * 2, family = "Consolas" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) "Pause"


canvas : List Renderable
canvas =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2d Area.area Area.fieldSize
    , text
    ]
