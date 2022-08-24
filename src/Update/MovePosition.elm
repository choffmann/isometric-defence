module Update.MovePosition exposing (update)

import Area exposing (Field(..))
import Messages exposing (GameArea(..), Msg)
import Model exposing (Model)
import Path exposing (Path(..))
import Pixel exposing (Pixel)
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
    (model.money >= price)
        && not (List.any (\tower -> tower.position == towerPoint) model.towers)
        && notOnPath model.path


update : Maybe Pixel -> GameArea -> Model -> ( Model, Cmd Msg )
update mPixel gameArea model =
    case gameArea of
        PlayArea ->
            ( case
                mPixel
                    |> Maybe.map (\pixel -> Area.pixelToField pixel)
                    |> Maybe.map (\(Field point) -> point)
              of
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

        ToolArea ->
            ( model, Cmd.none )
