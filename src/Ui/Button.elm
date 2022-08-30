module Ui.Button exposing (Button, drawSpriteButton, drawUiButton, onButton)

import Area exposing (Field)
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import Point exposing (Point)
import Ui.DrawUtils as DrawUtils


type alias Button =
    { position : Point
    , width : Float
    , height : Float
    }


drawUiButton : Button -> String -> Renderable
drawUiButton button text =
    let
        textPosition : Canvas.Point
        textPosition =
            ( (toFloat button.position.x * toFloat Area.fieldSize) + (button.width * toFloat Area.fieldSize) / 2
            , (toFloat button.position.y * toFloat Area.fieldSize) + (button.height * toFloat Area.fieldSize) / 2
            )
    in
    Canvas.group []
        [ Canvas.shapes [ Settings.fill Color.gray ] [ Canvas.rect (DrawUtils.fieldToCanvas button.position) (button.width * toFloat Area.fieldSize) (button.height * toFloat Area.fieldSize) ]
        , Canvas.text [ Text.font { size = 24, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle ] textPosition text
        ]


drawSpriteButton : Button -> Texture -> Renderable
drawSpriteButton button texture =
    Canvas.texture [] (DrawUtils.fieldToCanvas button.position) texture


onButton : Button -> Point -> Bool
onButton button point =
    (point.x >= button.position.x && point.x <= button.position.x + floor button.width - 1)
        && (point.y >= button.position.y && point.y <= button.position.y + floor button.height - 1)
