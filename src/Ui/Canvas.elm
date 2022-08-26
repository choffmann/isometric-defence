module Ui.Canvas exposing (..)

import Canvas exposing (Renderable)
import GameView exposing (GameView(..))
import Model exposing (Model)
import Ui.Screens.PlayScreen as PlayScreen


canvas : Model -> List Renderable
canvas model =
    case model.gameView of
        Isometric ->
            PlayScreen.isometricCanvas model

        TopDown ->
            PlayScreen.topDownCanvas model
