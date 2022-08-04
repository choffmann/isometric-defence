module Update.FullScreenChange exposing (update)

import Model exposing (Model)


update : Bool -> Model -> ( Model, Cmd msg )
update isFullScreen model =
    ( { model | fullscreen = isFullScreen }, Cmd.none )
