module Ui.Hud exposing (..)

import Area exposing (Field(..))
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


renderSprite : Field -> Texture -> Renderable
renderSprite field texture =
    Canvas.texture [] (DrawUtils.fieldToCanvas field) texture


drawBackground : Field -> Float -> Shape
drawBackground fromField width =
    Canvas.rect (DrawUtils.fieldToCanvas fromField) width (toFloat Area.fieldSize)


renderText : Field -> String -> Renderable
renderText (Field { x, y }) text =
    Canvas.text
        [ Text.font { size = 24, family = "Silkscreen" }, Text.align Left, Text.baseLine Middle ]
        ( toFloat ((x + 1) * Area.fieldSize), toFloat (y * Area.fieldSize) + (toFloat Area.fieldSize / 2) )
        text


position : Int -> Field
position index =
    Point (Area.widthTiles - ceiling (toFloat Area.widthTiles / 4)) index
        |> Field


backGroundWidth : Field -> Float
backGroundWidth (Field { x }) =
    toFloat ((Area.widthTiles - x) * Area.fieldSize)


drawInternal : Int -> Int -> Texture -> Renderable
drawInternal index amount texture =
    let
        internal field =
            Canvas.group []
                [ Canvas.shapes [ Settings.fill (Color.rgba 50 50 50 0.5) ]
                    [ backGroundWidth field
                        |> drawBackground field
                    ]
                , renderText field (String.fromInt amount)
                , renderSprite field texture
                ]
    in
    position index
        |> internal


drawHp : Int -> Texture -> Renderable
drawHp =
    drawInternal 1


drawCoin : Int -> Texture -> Renderable
drawCoin =
    drawInternal 0


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

        calcPoint : Field
        calcPoint =
            Point (ceiling (toFloat Area.widthTiles / 2) - (spriteWidth // 2)) 0
                |> Field
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

                StartScreenAnimation ->
                    Canvas.shapes [] []
