module Update.LeftClick exposing (update)

import Model exposing (Model)
import Point exposing (Point)


update : Maybe Point -> Model -> ( Model, Cmd msg )
update point model =
    ( { model | clicked = point }, Cmd.none )
