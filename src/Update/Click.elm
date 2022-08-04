module Update.Click exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Point exposing (Point)


update : Point -> Model -> ( Model, Cmd Msg )
update point model =
    ( model, Cmd.none )
