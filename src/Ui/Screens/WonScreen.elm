module Ui.Screens.WonScreen exposing (..)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color


text : Renderable
text =
    Canvas.text [ Text.font { size = Area.fieldSize * 2, family = "Consolas" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) "Won"


canvas : List Renderable
canvas =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , text
    ]
