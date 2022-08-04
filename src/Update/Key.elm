module Update.Key exposing (update)

import Messages exposing (Key(..), Msg)
import Model exposing (GameState(..), Model)


update : Messages.Key -> Model -> ( Model, Cmd Msg )
update key model =
    ( case key of
        Space ->
            case model.gameState of
                Running ->
                    { model | gameState = Paused }

                Paused ->
                    { model | gameState = Running }

                Won ->
                    model

                Lost ->
                    model

        UnknownKey ->
            model
    , Cmd.none
    )
