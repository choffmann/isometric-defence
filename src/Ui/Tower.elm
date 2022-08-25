module Ui.Tower exposing (pixelToTower, placingTowerToCanvas, renderPlacingTowerSprite, renderTowerSprite, towerArea, towerCanvas, towerRadius, towersToCanvas)

import Area exposing (Area, Field(..))
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Settings.Line
import Canvas.Settings.Text
import Canvas.Texture exposing (Texture)
import Color
import Model exposing (PlacingTower)
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Sprite exposing (TowerAreaSprite, TowerTexture)
import Tower exposing (Tower, Towers(..))
import Ui.DrawUtils as DrawUtils


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


renderPlacingTowerSprite : Maybe PlacingTower -> TowerTexture -> Texture -> List Renderable
renderPlacingTowerSprite maybePlacingTower texture towerCanNotPlaced =
    case maybePlacingTower of
        Nothing ->
            []

        Just placingTower ->
            if placingTower.canBePlaced then
                [ DrawUtils.placeTile (Point (placingTower.tower.position.x - 1) (placingTower.tower.position.y - 1))
                    (selectionToSprite placingTower.tower texture)
                ]

            else
                [ DrawUtils.placeTile (Point (placingTower.tower.position.x - 1) (placingTower.tower.position.y - 1)) towerCanNotPlaced ]


renderTowerSprite : List Tower -> TowerTexture -> List Renderable
renderTowerSprite towers texture =
    List.map (\tower -> DrawUtils.placeTile (Point (tower.position.x - 1) (tower.position.y - 1)) (towerToSprite tower texture)) towers


selectionToSprite : Tower -> TowerTexture -> Texture
selectionToSprite tower texture =
    case tower.towerType of
        Basic ->
            texture.basic.selection

        Tower1 ->
            texture.tower1.selection

        Tower2 ->
            texture.tower2.selection

        Tower3 ->
            texture.tower3.selection


towerToSprite : Tower -> TowerTexture -> Texture
towerToSprite tower texture =
    case tower.towerType of
        Basic ->
            texture.basic.tower

        Tower1 ->
            texture.tower1.tower

        Tower2 ->
            texture.tower2.tower

        Tower3 ->
            texture.tower3.tower


demoTowers : List Towers
demoTowers =
    [ Basic
    , Tower1
    , Tower2
    , Tower3
    ]


towerFieldSizeFactor : Float
towerFieldSizeFactor =
    2.5


towerSpriteWidth : Float
towerSpriteWidth =
    64


towerFieldSize : Float
towerFieldSize =
    toFloat Area.fieldSize * towerFieldSizeFactor


maxTowerAreaWidth : Int
maxTowerAreaWidth =
    (Area.area.width // Area.fieldSize) // floor towerFieldSizeFactor


maxTowerAreaHeight : List Towers -> Int
maxTowerAreaHeight towers =
    ceiling (toFloat (List.length towers) / toFloat maxTowerAreaWidth)


towerArea : Area
towerArea =
    Area Area.area.width (floor (towerFieldSize * toFloat (maxTowerAreaHeight demoTowers)))


towersToSelectArea : List Towers -> TowerAreaSprite -> List Renderable
towersToSelectArea towers texture =
    let
        currentHeight : Int -> Int
        currentHeight i =
            floor (toFloat i / toFloat maxTowerAreaWidth)

        canvasShape : Int -> Int -> Towers -> Renderable
        canvasShape i j tower =
            Canvas.group []
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.transform [] ]
                    -- Center Sprite in Tower Area Field
                    ( toFloat i * towerFieldSize + ((towerFieldSize - towerSpriteWidth) / 2), toFloat (currentHeight j) * towerFieldSize + 3 )
                    (case tower of
                        Basic ->
                            texture.basic

                        Tower1 ->
                            texture.tower1

                        Tower2 ->
                            texture.tower2

                        Tower3 ->
                            texture.tower3
                    )
                , Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "arial" } ] ( toFloat i * towerFieldSize + 3, (toFloat (currentHeight j) * towerFieldSize) + towerFieldSize - 3 ) (String.fromInt (Tower.toTower tower).price)
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


towerCanvas : TowerAreaSprite -> List Renderable
towerCanvas sprites =
    [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat towerArea.width) (toFloat towerArea.height) ]

    --, DrawUtils.drawCanvasGrid2d towerArea (floor towerFieldSize)
    ]
        ++ towersToSelectArea demoTowers sprites


pixelToTower : Pixel -> Maybe Towers
pixelToTower (Pixel point) =
    demoTowers
        |> List.drop (10 * (point.y // floor towerFieldSize) + (point.x // floor towerFieldSize))
        |> List.head
