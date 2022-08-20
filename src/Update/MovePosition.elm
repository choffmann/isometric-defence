module Update.MovePosition exposing (update)

import Messages exposing (Msg)
import Model exposing (Model)
import Path exposing (Path(..))
import Point exposing (Point)


canTowerBePlaced : Point -> Int -> Model -> Bool
canTowerBePlaced towerPoint price model =
    let
        notOnPath mPath =
            case mPath of
                Nothing ->
                    True

                Just (Last _ path) ->
                    not (List.any (\{ point } -> point == towerPoint) path)
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
