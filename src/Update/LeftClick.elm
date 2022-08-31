module Update.LeftClick exposing (update)

import Area exposing (Pixel)
import GameView exposing (GameView(..))
import Messages exposing (GameArea(..), Msg(..))
import Model exposing (GameState(..), Model, PlacingTower)
import Path
import Random
import Screen exposing (Screen(..))
import Tower
import Ui.Button as Button
import Ui.Hud as Hud
import Ui.Screens.HelpScreen as HelpScreen
import Ui.Screens.LostScreen as LostScreen
import Ui.Screens.PauseScreen as PauseScreen
import Ui.Screens.StartScreen as StartScreen
import Ui.Screens.WonScreen as WonScreen
import Ui.Tower


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

                                ( Just field, Nothing ) ->
                                    { model
                                        | clicked = Just field
                                        , inspectingTower = checkInspectTower field model.towers
                                    }

                                ( Just field, Just placingTower ) ->
                                    if placingTower.canBePlaced then
                                        { model
                                            | clicked = Just field
                                            , placingTower = Nothing
                                            , towers = placingTower.tower :: model.towers
                                            , nextTowerId = model.nextTowerId + 1
                                            , money = model.money - placingTower.tower.price
                                        }

                                    else
                                        { model | clicked = Just field }
                            , Cmd.none
                            )

                        WaitToStart ->
                            ( case
                                ( mPixel
                                    |> Maybe.map (Area.pixelToField model.gameView)
                                    |> Area.isOutOfBounds
                                , model.placingTower
                                )
                              of
                                ( Nothing, _ ) ->
                                    { model | clicked = Nothing }

                                ( Just field, Nothing ) ->
                                    if Button.onButton Hud.waitToStartButton field then
                                        { model | clicked = Just field, gameState = Running, inspectingTower = Nothing }

                                    else
                                        { model
                                            | clicked = Just field
                                            , inspectingTower = checkInspectTower field model.towers
                                        }

                                ( Just field, Just placingTower ) ->
                                    if Button.onButton Hud.waitToStartButton field then
                                        { model
                                            | clicked = Just field
                                            , placingTower = Nothing
                                            , gameState = Running
                                        }

                                    else if placingTower.canBePlaced then
                                        { model
                                            | clicked = Just field
                                            , placingTower = Nothing
                                            , towers = placingTower.tower :: model.towers
                                            , nextTowerId = model.nextTowerId + 1
                                            , money = model.money - placingTower.tower.price
                                        }

                                    else
                                        { model | clicked = Just field }
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
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just field ->
                            if Button.onButton StartScreen.startButton field then
                                ( { model | clicked = Just field, screen = PlayScreen }, Random.generate PathPointGenerate Path.pointGenerator )

                            else if Button.onButton StartScreen.helpButton field then
                                ( { model | clicked = Just field, screen = HelpScreen }, Random.generate PathPointGenerate Path.pointGenerator )

                            else
                                ( { model | clicked = Just field }, Cmd.none )

                PauseScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just field ->
                            if Button.onButton PauseScreen.resumeButton field then
                                ( { model | clicked = Just field, screen = PlayScreen, gameState = Running }, Cmd.none )

                            else
                                ( { model | clicked = Just field }, Cmd.none )

                WonScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just field ->
                            if Button.onButton WonScreen.restartButton field then
                                Model.restart model

                            else
                                ( { model | clicked = Just field }, Cmd.none )

                LostScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just field ->
                            if Button.onButton LostScreen.restartButton field then
                                Model.restart model

                            else
                                ( { model | clicked = Just field }, Cmd.none )

                HelpScreen ->
                    case
                        mPixel
                            |> Maybe.map (Area.pixelToField TopDown)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just field ->
                            if Button.onButton HelpScreen.backButton field then
                                ( { model | clicked = Just field, screen = StartScreen, gameState = StartScreenAnimation }, Cmd.none )

                            else
                                ( { model | clicked = Just field }, Cmd.none )

        ToolArea ->
            ( case mPixel of
                Nothing ->
                    { model | clicked = Nothing }

                Just pixel ->
                    { model
                        | clicked = Nothing
                        , placingTower =
                            Ui.Tower.pixelToTower pixel
                                |> Maybe.map (\tower -> PlacingTower (Tower.toTower model.nextTowerId tower) False)
                    }
            , Cmd.none
            )
