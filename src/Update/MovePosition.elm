module Update.MovePosition exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Point exposing (Point)


update : Maybe Point -> Model -> ( Model, Cmd Msg )
update mPoint model =
    ( case mPoint of
        Nothing ->
            model

        Just point ->
            { model
                | placingTower =
                    model.placingTower
                        |> Maybe.map
                            (\{ tower } ->
                                { tower = { tower | position = point }
                                , canBePlaced = True
                                }
                            )
            }
    , Cmd.none
    )
