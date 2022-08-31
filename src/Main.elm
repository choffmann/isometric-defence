module Main exposing (main)

import Area
import Browser
import Browser.Events
import Canvas
import Canvas.Settings
import Canvas.Texture as Texture
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseEnter)
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Screen exposing (Screen(..))
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
import Update.PathPointGenerate as PathPointGenerate
import Update.RightClick as RightClick
import Update.Texture
import Update.Tick as Tick
import Utils.Data exposing (Load(..))
import Utils.Decoder as Decoder
import Utils.Ports as Ports


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "./assets/tileset.png" TextureLoaded ]


onContextMenuEvent : Html.Attribute Msg
onContextMenuEvent =
    Html.Events.custom "contextmenu" Decoder.onContextMenuDecoder


view : Model -> Html Msg
view model =
    div (id "app" :: onContextMenuEvent :: Styles.appContainer)
        [ div Styles.canvasContainerStyles
            [ div
                (Html.Events.onMouseEnter Messages.EnterCanvas :: id "playAreaContainer" :: Styles.canvasStyles Area.area)
                [ Canvas.toHtmlWith
                    { width = Area.area.width, height = Area.area.height, textures = textures }
                    []
                    (Ui.Canvas.canvas model)
                ]
            ]
        , div Styles.canvasContainerStyles
            (case model.screen of
                PlayScreen ->
                    [ div
                        (onMouseEnter Messages.EnterCanvas :: id "toolAreaContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                        [ Canvas.toHtmlWith
                            { width = Ui.Tower.towerArea.width, height = Ui.Tower.towerArea.height, textures = textures }
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

                PauseScreen ->
                    [ div
                        (onMouseEnter Messages.EnterCanvas :: id "toolAreaContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                        [ Canvas.toHtml
                            ( Ui.Tower.towerArea.width, Ui.Tower.towerArea.height )
                            []
                            [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat Ui.Tower.towerArea.width) (toFloat Ui.Tower.towerArea.height) ] ]
                        ]
                    ]

                StartScreen ->
                    [ div
                        (onMouseEnter Messages.EnterCanvas :: id "toolAreaContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                        [ Canvas.toHtml
                            ( Ui.Tower.towerArea.width, Ui.Tower.towerArea.height )
                            []
                            [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat Ui.Tower.towerArea.width) (toFloat Ui.Tower.towerArea.height) ] ]
                        ]
                    ]

                WonScreen ->
                    [ div
                        (onMouseEnter Messages.EnterCanvas :: id "toolAreaContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                        [ Canvas.toHtml
                            ( Ui.Tower.towerArea.width, Ui.Tower.towerArea.height )
                            []
                            [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat Ui.Tower.towerArea.width) (toFloat Ui.Tower.towerArea.height) ] ]
                        ]
                    ]

                LostScreen ->
                    [ div
                        (onMouseEnter Messages.EnterCanvas :: id "toolAreaContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                        [ Canvas.toHtml
                            ( Ui.Tower.towerArea.width, Ui.Tower.towerArea.height )
                            []
                            [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat Ui.Tower.towerArea.width) (toFloat Ui.Tower.towerArea.height) ] ]
                        ]
                    ]

                HelpScreen ->
                    [ div
                        (onMouseEnter Messages.EnterCanvas :: id "toolAreaContainer" :: Styles.canvasStyles Ui.Tower.towerArea)
                        [ Canvas.toHtml
                            ( Ui.Tower.towerArea.width, Ui.Tower.towerArea.height )
                            []
                            [ Canvas.shapes [ Canvas.Settings.fill Color.grey ] [ Canvas.rect ( 0, 0 ) (toFloat Ui.Tower.towerArea.width) (toFloat Ui.Tower.towerArea.height) ] ]
                        ]
                    ]
            )
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
            GeneratePath.update direction

        PathPointGenerate field ->
            PathPointGenerate.update field

        TextureLoaded texture ->
            Update.Texture.update texture


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        always =
            [ Ports.onEventMessage ]

        generatePath =
            always

        startScreenAnimation =
            always
                ++ [ Browser.Events.onClick (Decoder.leftClickDecoder model)
                   , Browser.Events.onKeyDown Decoder.keyDecoder
                   , Browser.Events.onAnimationFrameDelta Tick
                   ]

        won =
            always
                ++ [ Browser.Events.onKeyDown Decoder.keyDecoder
                   , Browser.Events.onClick (Decoder.leftClickDecoder model)
                   ]

        lost =
            always
                ++ [ Browser.Events.onKeyDown Decoder.keyDecoder
                   , Browser.Events.onClick (Decoder.leftClickDecoder model)
                   ]

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

        waitToStart =
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
                paused

            Won ->
                won

            Lost ->
                lost

            GeneratePath ->
                generatePath

            WaitToStart ->
                case model.placingTower of
                    Just _ ->
                        Browser.Events.onMouseMove (Decoder.mouseMoveDecoder model) :: waitToStart

                    Nothing ->
                        waitToStart

            StartScreenAnimation ->
                startScreenAnimation
        )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> Model.init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
