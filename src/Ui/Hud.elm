module Ui.Hud exposing (..)

import Area
import Canvas exposing (Renderable, Shape)
import Canvas.Settings as Settings
import Canvas.Settings.Text as Text exposing (TextAlign(..), TextBaseLine(..))
import Canvas.Texture exposing (Texture)
import Color
import Model exposing (GameState(..))
import Point exposing (Point)
import Sprite exposing (ButtonSprites, Sprite)
import Ui.Button as Button exposing (Button)
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


waitToStartButton : Button
waitToStartButton =
    let
        spriteWidth : Int
        spriteWidth =
            4

        calcPoint : Point
        calcPoint =
            Point (ceiling (toFloat Area.widthTiles / 2) - (spriteWidth // 2)) 0
    in
    { position = calcPoint
    , width = toFloat spriteWidth
    , height = 1
    }


drawWaitToStartButton : GameState -> Load Sprite -> Renderable
drawWaitToStartButton gameState loadSprite =
    case loadSprite of
        Loading ->
            Canvas.shapes [] []

        Failure ->
            Canvas.shapes [] []

        Success sprites ->
            case gameState of
                WaitToStart ->
                    Button.drawSpriteButton
                        waitToStartButton
                        sprites.ui.buttons.start

                Running ->
                    Canvas.shapes [] []

                Paused ->
                    Canvas.shapes [] []

                GeneratePath ->
                    Canvas.shapes [] []

                Won ->
                    Canvas.shapes [] []

                Lost ->
                    Canvas.shapes [] []
