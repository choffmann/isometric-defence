module Ui.Tower exposing (placingTowerToCanvas, renderPlacingTowerSprite, renderTowerSprite, towerArea, towerCanvas, towerRadius, towersToCanvas)

import Area exposing (Area, Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Settings.Line
import Canvas.Settings.Text
import Canvas.Texture exposing (Texture)
import Color
import Model exposing (PlacingTower)
import Pixel
import Point exposing (Point)
import Tower exposing (Tower, Towers(..))
import Ui.DrawUtils as DrawUtils
import Ui.Sprites exposing (TowerSelectionSprite, TowerSprites)


towerRadius : List Tower -> Renderable
towerRadius towers =
    let
        centerPoint : Point -> Point
        centerPoint { x, y } =
            Point (x + (Area.fieldSize // 2)) (y + (Area.fieldSize // 2))

        towerPositionToPixel : Point -> Canvas.Point
        towerPositionToPixel point =
            Field point
                |> Area.fieldToPixel
                |> Pixel.pixelToPoint
                |> centerPoint
                |> Point.toCanvasPoint
                |> Area.canvasPointToIsometric
    in
    towers
        |> List.map (\tower -> Canvas.circle (towerPositionToPixel tower.position) tower.attackRadius)
        |> Canvas.shapes [ Canvas.Settings.stroke (Color.rgb255 0 0 0), Canvas.Settings.Line.lineWidth 2 ]


towersToCanvas : List Tower -> Renderable
towersToCanvas towers =
    towers
        |> List.map
            (\tower ->
                DrawUtils.pointToCanvas tower.position (toFloat Area.fieldSize) (toFloat Area.fieldSize)
            )
        |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 49 162 242) ]


placingTowerToCanvas : PlacingTower -> List Renderable
placingTowerToCanvas placingTower =
    [ Canvas.shapes
        [ Canvas.Settings.fill
            (if placingTower.canBePlaced then
                Color.green

             else
                Color.red
            )
        ]
        [ DrawUtils.pointToCanvas placingTower.tower.position (toFloat Area.fieldSize - 4) (toFloat Area.fieldSize - 4)
        ]
    , Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 49 162 242) ] [ DrawUtils.pointToCanvas placingTower.tower.position (toFloat Area.fieldSize - 20) (toFloat Area.fieldSize - 20) ]
    ]


renderPlacingTowerSprite : PlacingTower -> TowerSelectionSprite -> List Renderable
renderPlacingTowerSprite placingTower texture =
    if placingTower.canBePlaced then
        [ Canvas.texture []
            (Area.canvasPointToIsometric ( toFloat placingTower.tower.position.x, toFloat placingTower.tower.position.y )
                |> Area.isometricOffset
            )
            texture.towerCanPlaced
        ]

    else
        [ Canvas.texture []
            (Area.canvasPointToIsometric ( toFloat placingTower.tower.position.x, toFloat placingTower.tower.position.y )
                |> Area.isometricOffset
            )
            texture.towerCanNotPlaced
        ]


renderTowerSprite : List Tower -> Texture -> List Renderable
renderTowerSprite towers texture =
    towers
        |> List.map
            (\tower ->
                Canvas.texture []
                    (Area.canvasPointToIsometric ( toFloat tower.position.x - 1, toFloat tower.position.y - 1 )
                        |> Area.isometricOffset
                    )
                    texture
            )


demoTowers : List Towers
demoTowers =
    [ Basic
    , Tower1
    , Tower2
    , Tower3
    ]


towerFieldSizeFactor : Int
towerFieldSizeFactor =
    2


towerFieldSize : Int
towerFieldSize =
    Area.fieldSize * towerFieldSizeFactor


maxTowerAreaWidth : Int
maxTowerAreaWidth =
    (Area.area.width // Area.fieldSize) // towerFieldSizeFactor


maxTowerAreaHeight : List Towers -> Int
maxTowerAreaHeight towers =
    ceiling (toFloat (List.length towers) / toFloat maxTowerAreaWidth)


towerArea : Area
towerArea =
    Area Area.area.width (towerFieldSize * maxTowerAreaHeight demoTowers)


towersToSelectArea : List Towers -> TowerSprites -> List Renderable
towersToSelectArea towers sprites =
    let
        currentHeight : Int -> Int
        currentHeight i =
            floor (toFloat i / toFloat maxTowerAreaWidth)

        canvasShape : Int -> Int -> Towers -> Renderable
        canvasShape i j tower =
            let
                scale =
                    1.5
            in
            Canvas.group []
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.transform [ Canvas.Settings.Advanced.scale scale scale ] ]
                    -- durch scale teilen
                    ( toFloat i * toFloat towerFieldSize / scale, toFloat (currentHeight j * towerFieldSize) )
                    (case tower of
                        Basic ->
                            sprites.basic

                        Tower1 ->
                            sprites.tower1

                        Tower2 ->
                            sprites.tower1

                        Tower3 ->
                            sprites.tower1
                    )
                , Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "arial" } ] ( toFloat ((i * towerFieldSize) + 3), toFloat ((currentHeight j * towerFieldSize) + towerFieldSize - 3) ) (String.fromInt (Tower.toTower tower).price)
                ]

        draw : Int -> Int -> List Towers -> List Renderable -> List Renderable
        draw i j towerList list =
            case towerList of
                [] ->
                    list

                x :: xs ->
                    if i >= (maxTowerAreaWidth - 1) then
                        draw 0 (j + 1) xs (canvasShape i j x :: list)

                    else
                        draw (i + 1) (j + 1) xs (canvasShape i j x :: list)
    in
    draw 0 0 towers []


towerCanvas : TowerSprites -> List Renderable
towerCanvas sprites =
    [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat towerArea.width) (toFloat towerArea.height) ]
    , DrawUtils.drawCanvasGrid2d towerArea towerFieldSize
    ]
        ++ towersToSelectArea demoTowers sprites
