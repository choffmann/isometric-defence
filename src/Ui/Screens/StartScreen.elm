module Ui.Screens.StartScreen exposing (canvas, generateFloor, startButton)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import Point exposing (Point)
import Sprite exposing (Sprite)
import Ui.Animation as Animation exposing (Animation, Floor)
import Ui.Button as Button exposing (Button)
import Ui.DrawUtils as DrawUtils
import Ui.Sprites as Sprites
import Utils.Data exposing (Load(..))


gameTitle : String
gameTitle =
    "Isometric Defence"


title : Renderable
title =
    Canvas.text [ Text.font { size = 50, family = "Silkscreen" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) gameTitle


startButton : Button
startButton =
    { position = Point 8 16
    , width = 4
    , height = 2
    }


canvas : Load Sprite -> Maybe Animation -> List Renderable
canvas floorTexture mFloor =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2d Area.area Area.fieldSize
    , title
    , Button.drawUiButton startButton "Start"
    ]
        ++ (case mFloor of
                Nothing ->
                    []

                Just floor ->
                    case floorTexture of
                        Loading ->
                            []

                        Success texture ->
                            Animation.drawFloor texture.gameView.floor floor.floor

                        Failure ->
                            []
           )


generateFloor : List Floor
generateFloor =
    let
        drawWidth : List Floor -> Int -> Int -> Int -> List Floor
        drawWidth list i j count =
            if j >= Area.widthTiles then
                list

            else
                { position = ( toFloat (i + count), toFloat (j + count) )
                , matrix = Area.isometricMatrix
                , elapsedTime = 0
                }
                    :: drawWidth list i (j + 1) (count + 1)

        drawHeight : List Floor -> Int -> Int -> List Floor
        drawHeight list index count =
            if index >= Area.heightTiles then
                list

            else
                drawWidth [] index 0 (count + 1) ++ drawHeight list (index + 1) count

        draw : List Floor
        draw =
            drawHeight [] 0 0
    in
    draw
