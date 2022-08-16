module Main exposing (main)

import Area
import Browser
import Browser.Events
import Canvas exposing (PathSegment, Renderable, Shape)
import Canvas.Settings
import Canvas.Settings.Line
import Color
import Enemy exposing (Enemy)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Html.Events
import List.Extra as List
import List.Nonempty as Nonempty
import Messages exposing (Msg(..))
import Model exposing (Flags, GameState(..), Model, PlacingTower)
import Path exposing (Path)
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Styles
import Tower exposing (Tower)
import Update.Canvas as Canvas
import Update.EnterCanvas as EnterCanvas
import Update.Event as Event
import Update.GeneratePath as GeneratePath
import Update.Key as Key
import Update.LeftClick as LeftClick
import Update.MovePosition as MovePosition
import Update.RightClick as RightClick
import Update.Tick as Tick
import Utils.Decoder as Decoder
import Utils.Ports as Ports


drawCanvasGrid : Renderable
drawCanvasGrid =
    let
        drawLine : Float -> Float -> Float -> Float -> List PathSegment
        drawLine fromX fromY toX toY =
            [ Canvas.moveTo ( fromX, fromY ), Canvas.lineTo ( toX, toY ) ]

        drawWidth : List PathSegment -> Int -> List PathSegment
        drawWidth list index =
            if index == Area.area.height then
                list

            else
                drawLine (toFloat (index * Area.fieldSize)) 0 (toFloat (index * Area.fieldSize)) (toFloat Area.area.height)
                    |> List.append (drawWidth list (index + 1))

        drawHeight : List PathSegment -> Int -> List PathSegment
        drawHeight list index =
            if index == Area.area.width then
                drawWidth list 0

            else
                drawLine 0 (toFloat (index * Area.fieldSize)) (toFloat Area.area.width) (toFloat (index * Area.fieldSize))
                    |> List.append (drawHeight list (index + 1))

        draw : List PathSegment
        draw =
            drawHeight [] 0
    in
    Canvas.shapes [ Canvas.Settings.Line.lineWidth 1, Canvas.Settings.Line.lineDash [ 4 ] ] [ Canvas.path ( 0, 0 ) draw ]


pointToCanvas : Point -> Float -> Float -> Shape
pointToCanvas point width height =
    Canvas.rect ( toFloat (point.x * Area.fieldSize), toFloat (point.y * Area.fieldSize) ) width height


pathToCanvas : Maybe Path -> Renderable
pathToCanvas path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 255 50 50) ] (List.map (\pathPoint -> pointToCanvas pathPoint.point (toFloat Area.fieldSize) (toFloat Area.fieldSize)) (Nonempty.toList justPath))


enemiesToCanvas : List Enemy -> Maybe Path -> Renderable
enemiesToCanvas enemies path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            enemies
                |> List.map
                    (\enemy ->
                        Path.distanceToPixel justPath enemy.distance
                            |> Maybe.map
                                (\(Pixel point) ->
                                    Canvas.rect ( toFloat point.x - 10, toFloat point.y - 10 ) 20 20
                                )
                    )
                |> List.removeNothing
                |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 255 50) ]


towersToCanvas : List Tower -> Renderable
towersToCanvas towers =
    towers
        |> List.map
            (\tower ->
                pointToCanvas tower.position 20 20
            )
        |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 50 255) ]


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
        [ Canvas.rect ( toFloat (placingTower.tower.position.x * Area.fieldSize + 2), toFloat (placingTower.tower.position.y * Area.fieldSize + 2) ) (toFloat Area.fieldSize - 4) (toFloat Area.fieldSize - 4) ]
    , Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 50 255) ] [ Canvas.rect ( toFloat (placingTower.tower.position.x * Area.fieldSize + 10), toFloat (placingTower.tower.position.y * Area.fieldSize + 10) ) (toFloat Area.fieldSize - 20) (toFloat Area.fieldSize - 20) ]
    ]


canvas : Model -> List Renderable
canvas model =
    [ Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat Area.area.width) (toFloat Area.area.height) ]
    , drawCanvasGrid
    , pathToCanvas model.path
    , enemiesToCanvas model.enemies model.path
    , towersToCanvas model.towers
    ]
        ++ (case model.placingTower of
                Nothing ->
                    []

                Just placingTower ->
                    placingTowerToCanvas placingTower
           )


debugModel : Model -> Html Msg
debugModel model =
    div []
        [ div [] [ text (String.fromFloat model.delta) ]
        , div [] [ text "Canvas: ", text (Debug.toString model.canvas) ]
        , div [] [ text "CLicked: ", text (Debug.toString model.clicked) ]
        , div [] [ text "Gamestate: ", text (Debug.toString model.gameState) ]
        , div [] [ text "SpeedMult: ", text (Debug.toString model.speedMulti) ]
        , div [] [ text "HP: ", text (Debug.toString model.hp) ]
        , div [] [ text "Money: ", text (Debug.toString model.money) ]
        , div [] [ text "Fullscreen: ", text (Debug.toString model.fullscreen) ]
        , div [] [ text "PlacingTower: ", text (Debug.toString model.placingTower) ]
        , div [] [ text "Enemies: ", text (Debug.toString model.enemies) ]
        , div [] [ text "Towers: ", text (Debug.toString model.towers) ]

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
                (Html.Events.onMouseEnter Messages.EnterCanvas :: id "canvasContainer" :: Styles.canvasStyles)
                [ Canvas.toHtml
                    ( Area.area.width, Area.area.height )
                    []
                    (canvas model)
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
