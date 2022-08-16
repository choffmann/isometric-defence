module Update.MovePosition exposing (update)

import List.Nonempty as Nonempty
import Messages exposing (Msg)
import Model exposing (Model)
import Point exposing (Point)


canTowerBePlaced : Point -> Int -> Model -> Bool
canTowerBePlaced towerPoint price model =
    let
        notOnPath mPath =
            case mPath of
                Nothing ->
                    True

                Just path ->
                    not (List.any (\{ point } -> point == towerPoint) (Nonempty.toList path))
    in
    (model.money > price)
        && not (List.any (\tower -> tower.position == towerPoint) model.towers)
        && notOnPath model.path


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
                                , canBePlaced = canTowerBePlaced point tower.price model
                                }
                            )
            }
    , Cmd.none
    )
