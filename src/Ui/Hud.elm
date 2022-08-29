module Ui.Hud exposing (..)

import Area
import Canvas exposing (Renderable, Shape)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import Point exposing (Point)
import Sprite exposing (Sprite)
import Ui.Coin as Coin
import Ui.DrawUtils as DrawUtils
import Utils.Data exposing (Load(..))


drawCoin : Int -> Load Sprite -> List Renderable
drawCoin amount loadTexture =
    let
        position : Point
        position =
            Point (Area.widthTiles - ceiling (toFloat Area.widthTiles / 4)) 0

        drawBackground : Point -> Float -> Shape
        drawBackground fromPoint width =
            Canvas.rect (DrawUtils.convertToCanvasPoint fromPoint) width (toFloat Area.fieldSize)

        renderCoin : Texture -> List Renderable
        renderCoin texture =
            [ Coin.drawCoin position texture ]

        renderText : List Renderable
        renderText =
            [ Canvas.text
                [ Text.font { size = 24, family = "Silkscreen" }, Text.align Left, Text.baseLine Middle ]
                ( toFloat ((position.x + 1) * Area.fieldSize), toFloat position.y + (toFloat Area.fieldSize / 2) )
                (String.fromInt amount)
            ]
    in
    [ Canvas.shapes [ Settings.fill Color.gray ]
        [ drawBackground position (toFloat ((Area.widthTiles - position.x) * Area.fieldSize))
        ]
    ]
        ++ renderText
        ++ (case loadTexture of
                Loading ->
                    []

                Success sprite ->
                    renderCoin sprite.ui.coin

                Failure ->
                    []
           )
