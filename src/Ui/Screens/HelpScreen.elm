module Ui.Screens.HelpScreen exposing (backButton, canvas)

import Area exposing (Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Point exposing (Point)
import Ui.Button as Button exposing (Button)
import Ui.DrawUtils as DrawUtils


helpTitle : String
helpTitle =
    "How to Play"


text : Renderable
text =
    Canvas.text [ Text.font { size = 50, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) helpTitle


helpText : Renderable
helpText =
    let
        drawText : List String -> List Renderable
        drawText =
            let
                offset =
                    5
            in
            List.indexedMap
                (\i ->
                    Canvas.text [ Text.font { size = 24, family = "Silkscreen" }, Text.align Left ]
                        (DrawUtils.pointToFloat (Point Area.fieldSize ((offset + i) * Area.fieldSize)))
                )
    in
    Canvas.group []
        (drawText
            [ "Place Tower to defense the evil boxes"
            , ""
            , "Key Mapping:"
            , "Space        -> Start Game"
            , "F            -> Enter/Exit FullScreen"
            , "R            -> Restart Game"
            , "I            -> Change GameView"
            , "P            -> Pause Game"
            , "ArrowUp      -> Speed Up Game"
            , "ArrowDown    -> Speed Down Game"
            ]
        )


backButton : Button
backButton =
    { position = Field (Point 8 16)
    , width = 4
    , height = 2
    }


canvas : List Renderable
canvas =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , Button.drawUiButton backButton "Back"
    , text
    , helpText
    ]
