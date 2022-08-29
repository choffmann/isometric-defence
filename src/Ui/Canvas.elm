module Ui.Canvas exposing (..)

import Canvas exposing (Renderable)
import GameView exposing (GameView(..))
import Model exposing (Model)
import Screen exposing (Screen(..))
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
            case model.gameView of
                Isometric ->
                    PlayScreen.isometricCanvas model

                TopDown ->
                    PlayScreen.topDownCanvas model

        PauseScreen ->
            case model.gameView of
                Isometric ->
                    PlayScreen.isometricCanvas model ++ PauseScreen.canvas

                TopDown ->
                    PlayScreen.topDownCanvas model ++ PauseScreen.canvas

        WonScreen ->
            WonScreen.canvas

        LostScreen ->
            LostScreen.canvas
