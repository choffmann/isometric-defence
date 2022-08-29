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
import Ui.DrawUtils as DrawUtils
import Utils.Data exposing (Load(..))


renderSprite : Point -> Texture -> Renderable
renderSprite point texture =
    Canvas.texture [] (DrawUtils.convertToCanvasPoint point) texture


drawBackground : Point -> Float -> Shape
drawBackground fromPoint width =
    Canvas.rect (DrawUtils.convertToCanvasPoint fromPoint) width (toFloat Area.fieldSize)


renderText : Point -> String -> Renderable
renderText point text =
    Canvas.text
        [ Text.font { size = 24, family = "Silkscreen" }, Text.align Left, Text.baseLine Middle ]
        ( toFloat ((point.x + 1) * Area.fieldSize), toFloat (point.y * Area.fieldSize) + (toFloat Area.fieldSize / 2) )
        text


drawHp : Int -> Texture -> Renderable
drawHp hp texture =
    let
        position : Point
        position =
            Point (Area.widthTiles - ceiling (toFloat Area.widthTiles / 4)) 1
    in
    Canvas.group []
        [ Canvas.shapes [ Settings.fill (Color.rgba 50 50 50 0.5) ] [ drawBackground position (toFloat ((Area.widthTiles - position.x) * Area.fieldSize)) ]
        , renderText position (String.fromInt hp)
        , renderSprite position texture
        ]


drawCoin : Int -> Texture -> Renderable
drawCoin amount texture =
    let
        position : Point
        position =
            Point (Area.widthTiles - ceiling (toFloat Area.widthTiles / 4)) 0
    in
    Canvas.group []
        [ Canvas.shapes [ Settings.fill (Color.rgba 50 50 50 0.5) ]
            [ drawBackground position (toFloat ((Area.widthTiles - position.x) * Area.fieldSize)) ]
        , renderText position (String.fromInt amount)
        , renderSprite position texture
        ]


hud : Int -> Int -> Load Sprite -> Renderable
hud money hp loadTexture =
    case loadTexture of
        Loading ->
            Canvas.shapes [] []

        Success sprite ->
            Canvas.group []
                [ drawCoin money sprite.ui.coin
                , drawHp hp sprite.ui.heart
                ]

        Failure ->
            Canvas.shapes [] []


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
