module Update.MovePosition exposing (update)

import Area exposing (Field(..), Pixel(..))
import GameView exposing (GameView(..))
import Messages exposing (GameArea(..), Msg)
import Model exposing (Model)
import Point exposing (Point)
import Ui.Button as Button
import Ui.Hud as Hud


canTowerBePlaced : Point -> Int -> Model -> Bool
canTowerBePlaced towerPoint price model =
    let
        notOnPath mPath =
            case mPath of
                Nothing ->
                    True

                Just path ->
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
                    |> Maybe.map (Area.pixelToField model.gameView)
                    |> Area.isOutOfBounds
                    |> Maybe.map (\(Field point) -> point)
              of
                Nothing ->
                    model

                Just point ->
                    if Button.onButton Hud.waitToStartButton point then
                        { model | placingTower = Nothing }

                    else
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
