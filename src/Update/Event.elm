module Update.Event exposing (update)

import Messages exposing (ReceivingEvents(..))
import Model exposing (Model)


update : ReceivingEvents -> Model -> ( Model, Cmd msg )
update event model =
    case event of
        FullScreenChanged mode ->
            ( { model | fullscreen = mode }, Cmd.none )

        UnknownEvent ->
            ( model, Cmd.none )
