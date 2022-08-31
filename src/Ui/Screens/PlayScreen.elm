module Ui.Screens.PlayScreen exposing (isometricCanvas, topDownCanvas)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings
import Color
import Model exposing (Model)
import Ui.DrawUtils as DrawUtils
import Ui.Enemy
import Ui.FiredShot as FiredShot
import Ui.Hud
import Ui.Path
import Ui.Sprites
import Ui.Tower
import Utils.Data exposing (Load(..))


renderIsoSprites : Model -> List Renderable
renderIsoSprites model =
    case model.sprite of
        Loading ->
            [ Canvas.shapes [] [] ]

        Success sprites ->
            Ui.Sprites.renderFloorSprite sprites.gameView.isometric.floor
                ++ Ui.Path.renderPathSprite model.path sprites.gameView.isometric.path
                ++ Ui.Enemy.renderEnemyIso model.enemies model.path sprites.gameView.isometric.enemy
                ++ Ui.Tower.renderTowerSprite model.towers sprites.gameView.isometric.towers
                ++ Ui.Tower.renderPlacingTowerSprite model.placingTower sprites.gameView.isometric.towers sprites.gameView.isometric.towerCanNotPlaced

        Failure ->
            [ Canvas.shapes [] [] ]


renderTopDownSprites : Model -> List Renderable
renderTopDownSprites model =
    case model.sprite of
        Loading ->
            []

        Success sprites ->
            Ui.Enemy.enemiesToCanvas model.enemies sprites.gameView.topDown.enemy model.path

        Failure ->
            []


isometricCanvas : Model -> List Renderable
isometricCanvas model =
    Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
        :: renderIsoSprites model
        ++ FiredShot.drawShot model.gameView model.towers model.enemies model.shotsFired
        ++ [ Ui.Tower.towerRadius model.inspectingTower model.gameView ]


topDownCanvas : Model -> List Renderable
topDownCanvas model =
    [ Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 34 139 34) ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2D
    , Ui.Path.pathToCanvas model.path
    , Ui.Tower.towerRadius model.inspectingTower model.gameView
    , Ui.Hud.drawWaitToStartButton model.gameState model.sprite
    ]
        ++ Ui.Tower.towersToCanvas model.towers
        ++ (case model.placingTower of
                Nothing ->
                    []

                Just placingTower ->
                    Ui.Tower.placingTowerToCanvas placingTower
           )
        ++ renderTopDownSprites model
        ++ FiredShot.drawShot model.gameView model.towers model.enemies model.shotsFired
