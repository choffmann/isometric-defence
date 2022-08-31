module Ui.Canvas exposing (canvas)

import Canvas exposing (Renderable)
import GameView exposing (GameView(..))
import Model exposing (GameState(..), Model)
import Screen exposing (Screen(..))
import Ui.Hud as Hud
import Ui.Screens.LostScreen as LostScreen
import Ui.Screens.PauseScreen as PauseScreen
import Ui.Screens.PlayScreen as PlayScreen
import Ui.Screens.StartScreen as StartScreen
import Ui.Screens.WonScreen as WonScreen


canvas : Model -> List Renderable
canvas model =
    case model.screen of
        StartScreen ->
            StartScreen.canvas model.sprite model.animation

        PlayScreen ->
            case model.gameState of
                Running ->
                    case model.gameView of
                        Isometric ->
                            PlayScreen.isometricCanvas model
                                ++ [ Hud.hud model.money model.hp model.sprite
                                   , Hud.drawWaitToStartButton model.gameState model.sprite
                                   ]

                        TopDown ->
                            PlayScreen.topDownCanvas model
                                ++ [ Hud.hud model.money model.hp model.sprite
                                   , Hud.drawWaitToStartButton model.gameState model.sprite
                                   ]

                Paused ->
                    case model.gameView of
                        Isometric ->
                            PlayScreen.isometricCanvas model

                        TopDown ->
                            PlayScreen.topDownCanvas model

                Lost ->
                    []

                Won ->
                    []

                WaitToStart ->
                    case model.gameView of
                        Isometric ->
                            PlayScreen.isometricCanvas model
                                ++ [ Hud.hud model.money model.hp model.sprite
                                   , Hud.drawWaitToStartButton model.gameState model.sprite
                                   ]

                        TopDown ->
                            PlayScreen.topDownCanvas model
                                ++ [ Hud.hud model.money model.hp model.sprite
                                   , Hud.drawWaitToStartButton model.gameState model.sprite
                                   ]

                GeneratePath ->
                    []

                StartScreenAnimation ->
                    []

        PauseScreen ->
            case model.gameView of
                Isometric ->
                    PlayScreen.isometricCanvas model
                        ++ PauseScreen.canvas
                        ++ [ PauseScreen.drawGameStats model.money model.hp model.sprite ]

                TopDown ->
                    PlayScreen.topDownCanvas model
                        ++ PauseScreen.canvas
                        ++ [ PauseScreen.drawGameStats model.money model.hp model.sprite ]

        WonScreen ->
            WonScreen.canvas

        LostScreen ->
            LostScreen.canvas
