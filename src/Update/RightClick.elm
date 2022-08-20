module Update.RightClick exposing (update)

import Model exposing (Model)


update : Model -> ( Model, Cmd msg )
update model =
    ( case model.placingTower of
        Nothing ->
            model

        Just _ ->
            { model | placingTower = Nothing }
    , Cmd.none
    )
