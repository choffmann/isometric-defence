module Ui.Screens.WonScreen exposing (..)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Point exposing (Point)
import Ui.Button as Button exposing (Button)
import Ui.DrawUtils as DrawUtils


wonText : String
wonText =
    "Won"


text : Renderable
text =
    Canvas.text [ Text.font { size = 50, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) wonText


restartButton : Button
restartButton =
    { position = Point 8 16
    , width = 4
    , height = 2
    , text = "Restart"
    }


canvas : List Renderable
canvas =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2d Area.area Area.fieldSize
    , Button.draw restartButton
    , text
    ]
