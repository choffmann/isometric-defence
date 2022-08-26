module Main exposing (main)

import Area
import Browser
import Browser.Events
import Canvas exposing (Renderable)
import Canvas.Texture as Texture
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import Messages exposing (Msg(..))
import Model exposing (Flags, GameState(..), Model)
import Sprite exposing (IsometricViewSprite)
import Styles
import Ui.Canvas
import Ui.Tower
import Update.Canvas as Canvas
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.GeneratePath as GeneratePath
import Update.Key as Key
import Update.LeftClick as LeftClick
import Update.MovePosition as MovePosition
import Update.RightClick as RightClick
import Update.Screen
import Update.Texture
import Update.Tick as Tick
import Utils.Data exposing (Load(..))
import Utils.Decoder as Decoder
import Utils.Ports as Ports


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "./assets/tileset.png" TextureLoaded ]


debugModel : Model -> Html Msg
debugModel model =
    div []
        [ div [] [ text (String.fromFloat model.delta) ]
        , div [] [ text "PlayCanvas: ", text (Debug.toString model.playCanvas) ]
        , div [] [ text "ToolCanvas: ", text (Debug.toString model.toolCanvas) ]
        , div [] [ text "Clicked: ", text (Debug.toString model.clicked) ]
        , div [] [ text "Gamestate: ", text (Debug.toString model.gameState) ]
        , div [] [ text "SpeedMult: ", text (Debug.toString model.speedMulti) ]
        , div [] [ text "HP: ", text (Debug.toString model.hp) ]
        , div [] [ text "Money: ", text (Debug.toString model.money) ]
        , div [] [ text "Fullscreen: ", text (Debug.toString model.fullscreen) ]
        , div [] [ text "PlacingTower: ", text (Debug.toString model.placingTower) ]
        , div [] [ text "InspectingTower: ", text (Debug.toString model.inspectingTower) ]
        , div [] [ text "Enemies: ", text (Debug.toString model.enemies) ]
        , div [] [ text "Towers: ", text (Debug.toString model.towers) ]
        , div [] [ text "GameView: ", text (Debug.toString model.gameView) ]
        , div [] [ text "CurrentScreen: ", text (Debug.toString model.screen) ]

        --, div [] [ text "TowerAreaSprite: ", text (Debug.toString model.sprite) ]
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
                (Html.Events.onMouseEnter Messages.EnterCanvas :: id "playAreaContainer" :: Styles.canvasStyles Area.area)
                [ Canvas.toHtmlWith
                    { width = Area.area.width, height = Area.area.height, textures = textures }
                    []
                    (Ui.Canvas.canvas model)
                ]
            ]
        , div Styles.canvasContainerStyles
            [ div
                (onMouseEnter Messages.EnterCanvas :: id "toolAreaContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                [ Canvas.toHtmlWith
                    { width = Area.area.width, height = Area.area.height, textures = textures }
                    []
                    (case model.sprite of
                        Loading ->
                            [ Canvas.shapes [] [] ]

                        Success sprites ->
                            Ui.Tower.towerCanvas sprites.towerArea

                        Failure ->
                            [ Canvas.shapes [] [] ]
                    )
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

        LeftClick gameArea point ->
            LeftClick.update point gameArea

        RightClick ->
            RightClick.update

        MovePosition gameArea point ->
            MovePosition.update point gameArea

        Canvas gameArea maybe ->
            Canvas.update gameArea maybe

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

        ChangeScreen screen ->
            Update.Screen.update screen


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
                        Browser.Events.onMouseMove (Decoder.mouseMoveDecoder model) :: running

            Paused ->
                case model.placingTower of
                    Just _ ->
                        Browser.Events.onMouseMove (Decoder.mouseMoveDecoder model) :: paused

                    Nothing ->
                        Browser.Events.onMouseMove (Decoder.mouseMoveDecoder model) :: paused

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
