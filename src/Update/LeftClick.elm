module Update.LeftClick exposing (update)

import Area exposing (Field(..), Pixel)
import GameView exposing (GameView(..))
import Messages exposing (GameArea(..), Msg(..))
import Model exposing (GameState(..), Model, PlacingTower)
import Path
import Random
import Screen exposing (Screen(..))
import Tower exposing (Towers(..))
import Ui.Button as Button
import Ui.Hud as Hud
import Ui.Screens.LostScreen as LostScreen
import Ui.Screens.PauseScreen as PauseScreen
import Ui.Screens.StartScreen as StartScreen
import Ui.Screens.WonScreen as WonScreen
import Ui.Tower exposing (pixelToTower)


update : Maybe Pixel -> GameArea -> Model -> ( Model, Cmd Msg )
update mPixel gameArea model =
    let
        checkInspectTower point towers =
            case towers of
                [] ->
                    Nothing

                tower :: hs ->
                    if tower.position == point then
                        Just tower

                    else
                        checkInspectTower point hs
    in
    case gameArea of
        PlayArea ->
            case model.screen of
                PlayScreen ->
                    case model.gameState of
                        Running ->
                            ( case
                                ( mPixel
                                    |> Maybe.map (Area.pixelToField model.gameView)
                                    |> Area.isOutOfBounds
                                , model.placingTower
                                )
                              of
                                ( Nothing, _ ) ->
                                    { model | clicked = Nothing }

                                ( Just (Field point), Nothing ) ->
                                    { model
                                        | clicked = Just point
                                        , inspectingTower = checkInspectTower point model.towers
                                    }

                                ( Just (Field point), Just placingTower ) ->
                                    if placingTower.canBePlaced then
                                        { model
                                            | clicked = Just point
                                            , placingTower = Nothing
                                            , towers = placingTower.tower :: model.towers
                                            , nextTowerId = model.nextTowerId + 1
                                            , money = model.money - placingTower.tower.price
                                        }

                                    else
                                        { model | clicked = Just point }
                            , Cmd.none
                            )

                        WaitToStart ->
                            ( case
                                ( mPixel
                                    |> Maybe.map (Area.pixelToField model.gameView)
                                    |> Area.isOutOfBounds
                                    |> Maybe.map (\(Field point) -> point)
                                , model.placingTower
                                )
                              of
                                ( Nothing, _ ) ->
                                    { model | clicked = Nothing }

                                ( Just point, Nothing ) ->
                                    if Button.onButton Hud.waitToStartButton point then
                                        { model | clicked = Just point, gameState = Running, inspectingTower = Nothing }

                                    else
                                        { model
                                            | clicked = Just point
                                            , inspectingTower = checkInspectTower point model.towers
                                        }

                                ( Just point, Just placingTower ) ->
                                    if Button.onButton Hud.waitToStartButton point then
                                        { model
                                            | clicked = Just point
                                            , placingTower = Nothing
                                            , gameState = Running
                                        }

                                    else if placingTower.canBePlaced then
                                        { model
                                            | clicked = Just point
                                            , placingTower = Nothing
                                            , towers = placingTower.tower :: model.towers
                                            , nextTowerId = model.nextTowerId + 1
                                            , money = model.money - placingTower.tower.price
                                        }

                                    else
                                        { model | clicked = Just point }
                            , Cmd.none
                            )

                        Lost ->
                            ( model, Cmd.none )

                        Won ->
                            ( model, Cmd.none )

                        GeneratePath ->
                            ( model, Cmd.none )

                        Paused ->
                            ( model, Cmd.none )

                        StartScreenAnimation ->
                            ( model, Cmd.none )

                StartScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                            |> Maybe.map (\(Field point) -> point)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just point ->
                            if Button.onButton StartScreen.startButton point then
                                ( { model | clicked = Just point, screen = PlayScreen }, Random.generate PathPointGenerate Path.pointGenerator )

                            else
                                ( { model | clicked = Just point }, Cmd.none )

                PauseScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                            |> Maybe.map (\(Field point) -> point)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just point ->
                            if Button.onButton PauseScreen.resumeButton point then
                                ( { model | clicked = Just point, screen = PlayScreen, gameState = Running }, Cmd.none )

                            else
                                ( { model | clicked = Just point }, Cmd.none )

                WonScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                            |> Maybe.map (\(Field point) -> point)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just point ->
                            if Button.onButton WonScreen.restartButton point then
                                Model.restart model { msg = "" }

                            else
                                ( { model | clicked = Just point }, Cmd.none )

                LostScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                            |> Maybe.map (\(Field point) -> point)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just point ->
                            if Button.onButton LostScreen.restartButton point then
                                Model.restart model { msg = "" }

                            else
                                ( { model | clicked = Just point }, Cmd.none )

        ToolArea ->
            ( case mPixel of
                Nothing ->
                    { model | clicked = Nothing }

                Just pixel ->
                    { model
                        | clicked = Nothing
                        , placingTower =
                            pixelToTower pixel
                                |> Maybe.map (\tower -> PlacingTower (Tower.toTower model.nextTowerId tower) False)
                    }
            , Cmd.none
            )
