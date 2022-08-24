module Update.EnterCanvas exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Utils.Commands as Commands


update : Model -> ( Model, Cmd Msg )
update model =
    ( model, Cmd.batch [ Commands.getPlayAreaCanvas, Commands.getToolAreaCanvas ] )
