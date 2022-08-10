module Update.Key exposing (update)

import FullScreenMode exposing (FullScreenMode(..))
import Messages exposing (Key(..), Msg, SendingEvents(..))
import Model exposing (GameState(..), Model)
import Utils.Ports as Ports


update : Messages.Key -> Model -> ( Model, Cmd Msg )
update key model =
    case key of
        Space ->
            ( case model.gameState of
                Running ->
                    { model | gameState = Paused }

                Paused ->
                    { model | gameState = Running }

                Won ->
                    model

                Lost ->
                    model

                GeneratePath ->
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
            Model.init { msg = "" }

        ArrowDown ->
            ( { model | speedMulti = model.speedMulti - 0.2 }, Cmd.none )

        ArrowUp ->
            ( { model | speedMulti = model.speedMulti + 0.2 }, Cmd.none )

        UnknownKey ->
            ( model, Cmd.none )
