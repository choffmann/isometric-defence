module Ui.Button exposing (Button, draw, isClicked)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Point exposing (Point)
import Ui.DrawUtils as DrawUtils


type alias Button =
    { position : Point
    , width : Float
    , height : Float
    , text : String
    }


draw : Button -> Renderable
draw button =
    let
        textPosition : Canvas.Point
        textPosition =
            ( (toFloat button.position.x * toFloat Area.fieldSize) + (button.width * toFloat Area.fieldSize) / 2
            , (toFloat button.position.y * toFloat Area.fieldSize) + (button.height * toFloat Area.fieldSize) / 2
            )
    in
    Canvas.group []
        [ Canvas.shapes [ Settings.fill Color.gray ] [ Canvas.rect (DrawUtils.convertToCanvasPoint button.position) (button.width * toFloat Area.fieldSize) (button.height * toFloat Area.fieldSize) ]
        , Canvas.text [ Text.font { size = 24, family = "Consolas" }, Text.align Center, Text.baseLine Middle ] textPosition button.text
        ]


isClicked : Button -> Point -> Bool
isClicked button point =
    (point.x >= button.position.x && point.x <= button.position.x + floor button.width - 1)
        && (point.y >= button.position.y && point.y <= button.position.y + floor button.height - 1)
