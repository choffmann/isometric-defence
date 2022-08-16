module Update.MovePosition exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Point exposing (Point)


update : Maybe Point -> Model -> ( Model, Cmd Msg )
update mPoint model =
    case mPoint of
        Nothing ->
            ( model, Cmd.none )

        Just point ->
            ( { model
                | placingTower =
                    model.placingTower
                        |> Maybe.map (\tower -> { tower | position = point })
              }
            , Cmd.none
            )
