module Ui.Screens.PauseScreen exposing (..)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import Point exposing (Point)
import Sprite exposing (Sprite)
import Ui.Button as Button exposing (Button)
import Ui.DrawUtils as DrawUtils
import Ui.Hud as Hud
import Utils.Data exposing (Load(..))


resumeButton : Button
resumeButton =
    { position = Point 8 16
    , width = 4
    , height = 2
    }


drawGameStats : Int -> Int -> Load Sprite -> Renderable
drawGameStats money hp texture =
    let
        width : Int
        width =
            Area.widthTiles // 3

        pos1 : Point
        pos1 =
            Point ((Area.widthTiles // 2) - width // 2) (Area.heightTiles // 2 - 2)

        pos2 : Point
        pos2 =
            Point pos1.x (pos1.y + 1)
    in
    case texture of
        Loading ->
            Canvas.shapes [] []

        Success sprite ->
            Canvas.group []
                [ Canvas.shapes [ Settings.fill Color.gray ] [ Hud.drawBackground pos1 (toFloat (width * Area.fieldSize)) ]
                , Canvas.shapes [ Settings.fill Color.gray ] [ Hud.drawBackground pos2 (toFloat (width * Area.fieldSize)) ]
                , Hud.renderSprite pos1 sprite.ui.coin
                , Hud.renderSprite pos2 sprite.ui.heart
                , Hud.renderText pos1 (String.fromInt money)
                , Hud.renderText pos2 (String.fromInt hp)
                ]

        Failure ->
            Canvas.shapes [] []


text : Renderable
text =
    Canvas.text [ Text.font { size = 50, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) "Pause"


canvas : List Renderable
canvas =
    [ Canvas.shapes [ Settings.fill (Color.rgba 50 50 50 0.5) ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , text
    , Button.drawUiButton resumeButton "Resume"
    ]
