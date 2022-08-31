module Ui.Screens.LostScreen exposing (canvas, restartButton)

import Area exposing (Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Point exposing (Point)
import Ui.Button as Button exposing (Button)


lostText : String
lostText =
    "Lost"


text : Renderable
text =
    Canvas.text [ Text.font { size = 50, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) lostText


restartButton : Button
restartButton =
    { position = Field (Point 8 16)
    , width = 4
    , height = 2
    }


canvas : List Renderable
canvas =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , Button.drawUiButton restartButton "Restart"
    , text
    ]
