module Update.LeftClick exposing (update)

import Area exposing (Field(..))
import Messages exposing (GameArea(..))
import Model exposing (Model)
import Pixel exposing (Pixel)
import Point exposing (Point)


update : Maybe Pixel -> GameArea -> Model -> ( Model, Cmd msg )
update mPixel gameArea model =
    case gameArea of
        PlayArea ->
            ( case
                ( mPixel
                    |> Maybe.map (\pixel -> Area.pixelToField pixel)
                    |> Maybe.map (\(Field point) -> point)
                , model.placingTower
                )
              of
                ( Nothing, _ ) ->
                    { model | clicked = Nothing }

                ( Just point, Nothing ) ->
                    { model | clicked = Just point }

                ( Just point, Just placingTower ) ->
                    if placingTower.canBePlaced then
                        { model | clicked = Just point, placingTower = Nothing, towers = placingTower.tower :: model.towers, money = model.money - placingTower.tower.price }

                    else
                        { model | clicked = Just point }
            , Cmd.none
            )

        ToolArea ->
            ( model, Cmd.none )
