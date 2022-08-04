module Update.Click exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Point exposing (Point)
import Utils.Commands as Commands


update : Point -> Model -> ( Model, Cmd Msg )
update point model =
    ( { model | clicked = Just point }
    , case model.canvas of
        Nothing ->
            Commands.getCanvas

        Just _ ->
            Cmd.none
    )
