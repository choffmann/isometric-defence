module Update.MovePosition exposing (update)

import List.Nonempty as Nonempty
import Messages exposing (Msg)
import Model exposing (Model)
import Point exposing (Point)


canTowerBePlaced : Point -> Int -> Model -> Bool
canTowerBePlaced towerPoint price model =
    let
        checkOnPath mPath =
            case mPath of
                Nothing ->
                    False

                Just path ->
                    List.any (\{ point } -> point == towerPoint) (Nonempty.toList path)
    in
    if model.money < price then
        False

    else if List.any (\tower -> tower.position == towerPoint) model.towers then
        False

    else if checkOnPath model.path then
        False

    else
        True


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
