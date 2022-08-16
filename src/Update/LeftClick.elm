module Update.LeftClick exposing (update)

import Model exposing (Model)
import Point exposing (Point)


update : Maybe Point -> Model -> ( Model, Cmd msg )
update point model =
    ( case model.placingTower of
        Nothing ->
            { model | clicked = point }

        Just placingTower ->
            if placingTower.canBePlaced then
                { model | clicked = point, placingTower = Nothing, towers = placingTower.tower :: model.towers, money = model.money - placingTower.tower.price }

            else
                { model | clicked = point }
    , Cmd.none
    )
