module Update.Click exposing (update)

import Model exposing (Model)
import Point exposing (Point)


update : Point -> Model -> ( Model, Cmd msg )
update point model =
    ( { model | clicked = Just point }, Cmd.none )
