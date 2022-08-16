module Update.MovePosition exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Point exposing (Point)


update : Maybe Point -> Model -> ( Model, Cmd Msg )
update mPoint model =
    ( model, Cmd.none )
