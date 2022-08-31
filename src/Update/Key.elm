module Update.Key exposing (update)

import FullScreenMode exposing (FullScreenMode(..))
import GameView exposing (GameView(..))
import Messages exposing (Key(..), Msg, SendingEvents(..))
import Model exposing (GameState(..), Model)
import Screen exposing (Screen(..))
import Utils.Commands as Commands
import Utils.Ports as Ports


update : Messages.Key -> Model -> ( Model, Cmd Msg )
update key model =
    case key of
        Space ->
            ( case model.gameState of
                Running ->
                    model

                Paused ->
                    model

                Won ->
                    model

                Lost ->
                    model

                GeneratePath ->
                    model

                StartScreenAnimation ->
                    model

                WaitToStart ->
                    { model | gameState = Running, screen = PlayScreen }
            , Cmd.none
            )

        P ->
            ( case model.gameState of
                Running ->
                    { model | gameState = Paused, screen = PauseScreen }

                Paused ->
                    { model | gameState = Running, screen = PlayScreen }

                Won ->
                    model

                Lost ->
                    model

                GeneratePath ->
                    model

                WaitToStart ->
                    model

                StartScreenAnimation ->
                    model
            , Cmd.none
            )

        F ->
            case model.fullscreen of
                Open ->
                    ( model, Ports.changeFullScreen (ChangeFullScreen Close) )

                Close ->
                    ( model, Ports.changeFullScreen (ChangeFullScreen Open) )

        R ->
            Model.restart model

        I ->
            ( case model.gameView of
                TopDown ->
                    { model | gameView = Isometric }

                Isometric ->
                    { model | gameView = TopDown }
            , Cmd.batch [ Commands.getPlayAreaCanvas, Commands.getToolAreaCanvas ]
            )

        ArrowDown ->
            ( { model | speedMulti = max 0 (model.speedMulti - 0.5) }, Cmd.none )

        ArrowUp ->
            ( { model | speedMulti = min 4 (model.speedMulti + 0.5) }, Cmd.none )

        UnknownKey ->
            ( model, Cmd.none )
