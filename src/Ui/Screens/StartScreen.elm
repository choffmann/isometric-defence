module Ui.Screens.StartScreen exposing (canvas, generateFloor, helpButton, startButton)

import Area exposing (Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Point exposing (Point)
import Sprite exposing (Sprite)
import Ui.Animation as Animation exposing (Animation, Floor)
import Ui.Button as Button exposing (Button)
import Utils.Data exposing (Load(..))


gameTitle : String
gameTitle =
    "Isometric Defence"


title : Renderable
title =
    Canvas.text [ Text.font { size = 50, family = "JetBrains Mono" }, Text.align Center, Text.baseLine Middle ] ( toFloat Area.area.width / 2, 70 ) gameTitle


startButton : Button
startButton =
    { position = Field (Point 8 11)
    , width = 4
    , height = 2
    }


helpButton : Button
helpButton =
    { position = Field (Point 8 15)
    , width = 4
    , height = 2
    }


canvas : Load Sprite -> Maybe Animation -> List Renderable
canvas floorTexture mFloor =
    [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , Button.drawUiButton startButton "Start"
    , Button.drawUiButton helpButton "Help"
    ]
        ++ (case mFloor of
                Nothing ->
                    []

                Just floor ->
                    case floorTexture of
                        Loading ->
                            []

                        Success texture ->
                            Animation.drawFloor texture.gameView.isometric.floor floor.floor

                        Failure ->
                            []
           )
        ++ [ title ]


generateFloor : List Floor
generateFloor =
    let
        offset =
            -5

        drawWidth : List Floor -> Int -> Int -> List Floor
        drawWidth list i j =
            if j >= Area.widthTiles then
                list

            else
                { position = ( toFloat i + offset, toFloat j + offset )
                , matrix = Area.isometricMatrix
                , elapsedTime = 0
                }
                    :: drawWidth list i (j + 1)

        drawHeight : List Floor -> Int -> List Floor
        drawHeight list index =
            if index >= Area.heightTiles then
                list

            else
                drawWidth [] index 0 ++ drawHeight list (index + 1)
    in
    drawHeight [] 0
