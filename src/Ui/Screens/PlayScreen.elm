module Ui.Screens.PlayScreen exposing (isometricCanvas, topDownCanvas)

import Area
import Canvas exposing (Renderable)
import Canvas.Settings
import Color
import Model exposing (Model)
import Sprite exposing (IsometricViewSprite, Sprite)
import Ui.DrawUtils as DrawUtils
import Ui.Enemy
import Ui.Path
import Ui.Sprites
import Ui.Tower
import Utils.Data exposing (Load(..))


renderSprites : Model -> IsometricViewSprite -> List Renderable
renderSprites model sprites =
    Ui.Sprites.renderFloorSprite sprites.floor
        ++ Ui.Path.renderPathSprite model.path sprites.path
        ++ Ui.Tower.renderTowerSprite model.towers sprites.towers
        ++ Ui.Enemy.renderEnemyIso model.enemies model.path sprites.enemy
        ++ Ui.Tower.renderPlacingTowerSprite model.placingTower sprites.towers sprites.towerCanNotPlaced


isometricCanvas : Model -> List Renderable
isometricCanvas model =
    [ Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    ]
        ++ (case model.sprite of
                Loading ->
                    [ Canvas.shapes [] [] ]

                Success sprites ->
                    renderSprites model sprites.gameView

                Failure ->
                    [ Canvas.shapes [] [] ]
           )


topDownCanvas : Model -> List Renderable
topDownCanvas model =
    [ Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 34 139 34) ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2d Area.area Area.fieldSize
    , Ui.Path.pathToCanvas model.path
    , Ui.Enemy.enemiesToCanvas model.enemies model.path
    , Ui.Tower.towersToCanvas model.towers
    , Ui.Tower.towerRadius model.inspectingTower model.gameView
    ]
        ++ (case model.placingTower of
                Nothing ->
                    []

                Just placingTower ->
                    Ui.Tower.placingTowerToCanvas placingTower
           )
