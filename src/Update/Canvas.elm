module Update.Canvas exposing (update)

import Browser.Dom exposing (Element)
import Messages exposing (Msg)
import Model exposing (Model)


update : Maybe Element -> Model -> ( Model, Cmd Msg )
update element model =
    ( model, Cmd.none )
