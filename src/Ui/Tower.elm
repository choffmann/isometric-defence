module Ui.Tower exposing (pixelToTower, placingTowerToCanvas, renderPlacingTowerSprite, renderTowerSprite, towerArea, towerCanvas, towerRadius, towersToCanvas)

import Area exposing (Area, Field(..), Pixel(..))
import Canvas exposing (Renderable, Shape)
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Settings.Line
import Canvas.Settings.Text
import Canvas.Texture exposing (Texture)
import Color
import GameView exposing (GameView(..))
import Model exposing (PlacingTower)
import Point exposing (Point)
import Sprite exposing (TowerAreaSprite, TowerTexture)
import Tower exposing (Tower, Towers(..))
import Ui.DrawUtils as DrawUtils


towerRadius : Maybe Tower -> GameView -> Renderable
towerRadius mTower gameView =
    let
        centerPoint : Pixel -> Pixel
        centerPoint (Pixel { x, y }) =
            Point (x + (Area.fieldSize // 2)) (y + (Area.fieldSize // 2))
                |> Pixel

        towerPositionToPixel : Field -> Canvas.Point
        towerPositionToPixel field =
            field
                |> Area.fieldToPixel
                |> centerPoint
                |> Area.pixelToPoint
                |> DrawUtils.pointToFloat

        centerIsoPoint : Point -> Canvas.Point
        centerIsoPoint { x, y } =
            ( toFloat x - 0.5, toFloat y - 0.5 )

        iso : Float -> Canvas.Point -> List Shape
        iso radius ( x, y ) =
            [ Canvas.path (Area.isometricOffset (Area.canvasPointToIsometric Area.isometricMatrix ( x, y )))
                [ Canvas.lineTo (Area.isometricOffset (Area.canvasPointToIsometric Area.isometricMatrix ( x, y - radius )))
                , Canvas.lineTo (Area.isometricOffset (Area.canvasPointToIsometric Area.isometricMatrix ( x + radius, y - radius )))
                , Canvas.lineTo (Area.isometricOffset (Area.canvasPointToIsometric Area.isometricMatrix ( x + radius, y )))
                , Canvas.lineTo (Area.isometricOffset (Area.canvasPointToIsometric Area.isometricMatrix ( x, y )))
                ]
            ]
    in
    Canvas.shapes [ Canvas.Settings.stroke (Color.rgb255 0 0 0), Canvas.Settings.Line.lineWidth 2 ]
        (case mTower of
            Nothing ->
                []

            Just tower ->
                case gameView of
                    TopDown ->
                        [ Canvas.circle
                            (towerPositionToPixel tower.position)
                            (tower.attackRadius * toFloat Area.fieldSize)
                        ]

                    Isometric ->
                        tower.position
                            |> Area.fieldToPoint
                            --|> centerIsoPoint
                            |> DrawUtils.pointToFloat
                            |> iso 3
        )


towerColor : Towers -> Canvas.Settings.Setting
towerColor tower =
    case tower of
        Basic ->
            Canvas.Settings.fill (Color.rgb255 49 162 242)

        Tower1 ->
            Canvas.Settings.fill (Color.rgb255 253 200 75)

        Tower2 ->
            Canvas.Settings.fill (Color.rgb255 158 117 85)

        Tower3 ->
            Canvas.Settings.fill (Color.rgb255 3 3 67)

        Tower4 ->
            Canvas.Settings.fill (Color.rgb255 3 3 67)


towersToCanvas : List Tower -> List Renderable
towersToCanvas towers =
    towers
        |> List.map
            (\tower ->
                Canvas.shapes
                    [ towerColor tower.towerType ]
                    [ DrawUtils.pointToCanvas tower.position (toFloat Area.fieldSize) (toFloat Area.fieldSize) ]
            )



--Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 49 162 242) ]


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
    , Canvas.shapes [ towerColor placingTower.tower.towerType ] [ DrawUtils.pointToCanvas placingTower.tower.position (toFloat Area.fieldSize - 20) (toFloat Area.fieldSize - 20) ]
    , towerRadius (Just placingTower.tower) TopDown
    ]


renderPlacingTowerSprite : Maybe PlacingTower -> TowerTexture -> Texture -> List Renderable
renderPlacingTowerSprite maybePlacingTower texture towerCanNotPlaced =
    let
        towerPositionToCanvasPoint (Field { x, y }) =
            Point (x - 1) (y - 1)
                |> DrawUtils.pointToFloat
    in
    case maybePlacingTower of
        Nothing ->
            []

        Just placingTower ->
            if placingTower.canBePlaced then
                [ DrawUtils.placeIsometricTile
                    (placingTower.tower.position
                        |> towerPositionToCanvasPoint
                    )
                    (selectionToSprite placingTower.tower texture)
                , towerRadius (Just placingTower.tower) Isometric
                ]

            else
                [ DrawUtils.placeIsometricTile
                    (placingTower.tower.position
                        |> towerPositionToCanvasPoint
                    )
                    towerCanNotPlaced
                ]


renderTowerSprite : List Tower -> TowerTexture -> List Renderable
renderTowerSprite towers texture =
    let
        towerPositionToCanvasPoint (Field { x, y }) =
            Point (x - 1) (y - 1)
                |> DrawUtils.pointToFloat
    in
    List.map
        (\tower ->
            DrawUtils.placeIsometricTile
                (towerPositionToCanvasPoint tower.position)
                (towerToSprite tower texture)
        )
        towers


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

        Tower4 ->
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

        Tower4 ->
            texture.tower3.tower


availableTowers : List Towers
availableTowers =
    [ Basic
    , Tower1
    , Tower2
    , Tower3
    , Tower4
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
    Area Area.area.width (floor (towerFieldSize * toFloat (maxTowerAreaHeight availableTowers)))


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

                        Tower4 ->
                            texture.tower3
                    )
                , Canvas.text [ Canvas.Settings.Text.font { size = 12, family = "Silkscreen" } ] ( toFloat i * towerFieldSize + 3, (toFloat (currentHeight j) * towerFieldSize) + towerFieldSize - 3 ) (String.fromInt (Tower.toTower 0 tower).price)
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
    Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat towerArea.width) (toFloat towerArea.height) ]
        :: towersToSelectArea availableTowers sprites


pixelToTower : Pixel -> Maybe Towers
pixelToTower (Pixel point) =
    availableTowers
        |> List.drop (10 * (point.y // floor towerFieldSize) + (point.x // floor towerFieldSize))
        |> List.head
