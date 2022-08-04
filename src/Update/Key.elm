module Update.Key exposing (update)

import Messages exposing (Key(..), Msg)
import Model exposing (GameState(..), Model)


update : Messages.Key -> Cmd Msg -> Cmd Msg -> Model -> ( Model, Cmd Msg )
update key enterFullScreen closeFullScreen model =
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
            , Cmd.none
            )

        F ->
            if model.fullscreen then
                ( { model | fullscreen = False }, closeFullScreen )

            else
                ( { model | fullscreen = True }, enterFullScreen )

        UnknownKey ->
            ( model, Cmd.none )
