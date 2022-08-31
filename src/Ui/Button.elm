module Ui.Button exposing (Button, drawSpriteButton, drawUiButton, onButton)

import Area exposing (Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import Ui.DrawUtils as DrawUtils


type alias Button =
    { position : Field
    , width : Float
    , height : Float
    }


drawUiButton : Button -> String -> Renderable
drawUiButton button text =
    let
        textPosition : Field -> Canvas.Point
        textPosition (Field point) =
            ( (toFloat point.x * toFloat Area.fieldSize) + (button.width * toFloat Area.fieldSize) / 2
            , (toFloat point.y * toFloat Area.fieldSize) + (button.height * toFloat Area.fieldSize) / 2
            )
    in
    Canvas.group []
        [ Canvas.shapes [ Settings.fill Color.gray ] [ Canvas.rect (DrawUtils.fieldToCanvas button.position) (button.width * toFloat Area.fieldSize) (button.height * toFloat Area.fieldSize) ]
        , Canvas.text [ Text.font { size = 24, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle ] (textPosition button.position) text
        ]


drawSpriteButton : Button -> Texture -> Renderable
drawSpriteButton button =
    Canvas.texture [] (DrawUtils.fieldToCanvas button.position)


onButton : Button -> Field -> Bool
onButton button (Field point) =
    let
        internal (Field buttonPoint) =
            (point.x >= buttonPoint.x && point.x <= buttonPoint.x + floor button.width - 1)
                && (point.y >= buttonPoint.y && point.y <= buttonPoint.y + floor button.height - 1)
    in
    internal button.position
