module Update.EnterCanvas exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Utils.Commands exposing (getCanvas)


update : Model -> ( Model, Cmd Msg )
update model =
    ( model, getCanvas )
