module Ui.Screens.StartScreen exposing (canvas, startButton)

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
import Ui.Sprites as Sprites
import Utils.Data exposing (Load(..))


gameTitle : String
gameTitle =
    "Isometric Defence"


title : Renderable
title =
    Canvas.text [ Text.font { size = Area.fieldSize * 2, family = "Consolas" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) gameTitle


startButton : Button
startButton =
    { position = Point 8 16
    , width = 4
    , height = 2
    , text = "Start"
    }


canvas : Load Sprite -> List Renderable
canvas floorTexture =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2d Area.area Area.fieldSize
    , title
    , Button.draw startButton
    ]
        ++ (case floorTexture of
                Loading ->
                    []

                Success texture ->
                    Sprites.renderFloorSprite texture.gameView.floor

                Failure ->
                    []
           )
