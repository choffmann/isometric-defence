module Main exposing (main)

import Area
import Browser
import Browser.Events
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Texture as Texture
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import Messages exposing (Msg(..))
import Model exposing (Flags, GameState(..), Model, PlacingTower)
import Styles
import Ui.DrawUtils as DrawUtils
import Ui.Enemy
import Ui.Path
import Ui.Tower
import Update.Canvas as Canvas
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.GeneratePath as GeneratePath
import Update.Key as Key
import Update.LeftClick as LeftClick
import Update.MovePosition as MovePosition
import Update.RightClick as RightClick
import Update.Texture
import Update.Tick as Tick
import Utils.Data exposing (Load(..))
import Utils.Decoder as Decoder
import Utils.Ports as Ports


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "./assets/isometric_ground.png" TextureLoaded ]


canvas : Model -> List Renderable
canvas model =
    [ Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , DrawUtils.drawCanvasGrid2d
    , DrawUtils.drawCanvasGrid
    , Ui.Path.pathToCanvas model.path
    , Ui.Enemy.enemiesToCanvas model.enemies model.path
    , Ui.Tower.towersToCanvas model.towers
    , Ui.Tower.towerRadius model.towers
    ]
        ++ (case model.placingTower of
                Nothing ->
                    []

                Just placingTower ->
                    Ui.Tower.placingTowerToCanvas placingTower
           )
        ++ (case model.sprites of
                Loading ->
                    [ Canvas.shapes [] [] ]

                Success sprites ->
                    [ Canvas.texture [] ( 0, 0 ) sprites.floor ]

                Failure ->
                    [ Canvas.shapes [] [] ]
           )


debugModel : Model -> Html Msg
debugModel model =
    div []
        [ div [] [ text (String.fromFloat model.delta) ]
        , div [] [ text "Canvas: ", text (Debug.toString model.canvas) ]
        , div [] [ text "Clicked: ", text (Debug.toString model.clicked) ]
        , div [] [ text "Gamestate: ", text (Debug.toString model.gameState) ]
        , div [] [ text "SpeedMult: ", text (Debug.toString model.speedMulti) ]
        , div [] [ text "HP: ", text (Debug.toString model.hp) ]
        , div [] [ text "Money: ", text (Debug.toString model.money) ]
        , div [] [ text "Fullscreen: ", text (Debug.toString model.fullscreen) ]
        , div [] [ text "PlacingTower: ", text (Debug.toString model.placingTower) ]
        , div [] [ text "Enemies: ", text (Debug.toString model.enemies) ]
        , div [] [ text "Towers: ", text (Debug.toString model.towers) ]
        , div [] [ text "Sprites: ", text (Debug.toString model.sprites) ]

        --, div [] [ text "Path: ", text (Debug.toString model.path) ]
        ]


onContextMenuEvent : Html.Attribute Msg
onContextMenuEvent =
    Html.Events.custom "contextmenu" Decoder.onContextMenuDecoder


view : Model -> Html Msg
view model =
    div (id "app" :: onContextMenuEvent :: Styles.appContainer)
        [ debugModel model
        , div Styles.canvasContainerStyles
            [ div
                (Html.Events.onMouseEnter Messages.EnterCanvas :: id "canvasContainer" :: Styles.canvasStyles Area.area)
                [ Canvas.toHtmlWith
                    { width = Area.area.width * 2, height = Area.area.height + Area.fieldSize, textures = textures }
                    --( Area.area.width * 2, Area.area.height + Area.fieldSize )
                    []
                    (canvas model)
                ]
            ]
        , div Styles.canvasContainerStyles
            [ div
                (onMouseEnter Messages.EnterCanvas :: id "canvasContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                [ Canvas.toHtml
                    ( Ui.Tower.towerArea.width, Ui.Tower.towerArea.height )
                    []
                    Ui.Tower.towerCanvas
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        Tick delta ->
            Tick.update delta

        Key key ->
            Key.update key

        LeftClick point ->
            LeftClick.update point

        RightClick ->
            RightClick.update

        MovePosition point ->
            MovePosition.update point

        Canvas maybe ->
            Canvas.update maybe

        EnterCanvas ->
            EnterCanvas.update

        Event event ->
            Event.update event

        PathDirectionGenerate direction ->
            GeneratePath.update (PathDirectionGenerate direction)

        PathPointGenerate point ->
            GeneratePath.update (PathPointGenerate point)

        TextureLoaded texture ->
            Update.Texture.update texture


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        always =
            [ Ports.onEventMessage ]

        generatePath =
            always

        won =
            always ++ [ Browser.Events.onKeyDown Decoder.keyDecoder ]

        lost =
            always ++ [ Browser.Events.onKeyDown Decoder.keyDecoder ]

        running =
            always
                ++ [ Browser.Events.onKeyDown Decoder.keyDecoder
                   , Browser.Events.onClick (Decoder.leftClickDecoder model)
                   , Browser.Events.onAnimationFrameDelta Tick
                   ]

        paused =
            always
                ++ [ Browser.Events.onKeyDown Decoder.keyDecoder
                   , Browser.Events.onClick (Decoder.leftClickDecoder model)
                   ]
    in
    Sub.batch
        (case model.gameState of
            Running ->
                case model.placingTower of
                    Just _ ->
                        Browser.Events.onMouseMove (Decoder.mouseMoveDecoder model) :: running

                    Nothing ->
                        running

            Paused ->
                case model.placingTower of
                    Just _ ->
                        Browser.Events.onMouseMove (Decoder.mouseMoveDecoder model) :: paused

                    Nothing ->
                        paused

            Won ->
                won

            Lost ->
                lost

            GeneratePath ->
                generatePath
        )


main : Program Flags Model Msg
main =
    Browser.element
        { init = Model.init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
