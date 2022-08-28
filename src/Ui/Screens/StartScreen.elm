module Ui.Screens.StartScreen exposing (canvas, startButton)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Point exposing (Point)
import Ui.Button as Button exposing (Button)
import Ui.DrawUtils as DrawUtils


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



{- startButton : Renderable
   startButton =
       let
           w : Float
           w =
               128

           h : Float
           h =
               40

           x : Float
           x =
               (toFloat Area.area.width / 2) - w / 2

           y : Float
           y =
               toFloat Area.area.height - h * 3
       in
       Canvas.group []
           [ Canvas.shapes [ Settings.fill Color.gray ] [ Canvas.rect ( x, y ) w h ]
           , Canvas.text [ Text.font { size = 24, family = "Consolas" }, Text.align Center, Text.baseLine Middle ] ( x + w / 2, y + 20 ) "Start"
           ]
-}


canvas : List Renderable
canvas =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2d Area.area Area.fieldSize
    , title
    , Button.draw startButton
    ]
