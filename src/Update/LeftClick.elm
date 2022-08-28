module Update.LeftClick exposing (update)

import Area exposing (Field(..))
import Messages exposing (GameArea(..))
import Model exposing (Model, PlacingTower)
import Pixel exposing (Pixel)
import Screen exposing (Screen(..))
import Tower exposing (Towers(..))
import Ui.Button as Button
import Ui.Screens.StartScreen as StartScreen
import Ui.Tower exposing (pixelToTower)


update : Maybe Pixel -> GameArea -> Model -> ( Model, Cmd msg )
update mPixel gameArea model =
    let
        checkInspectTower point towers =
            case towers of
                [] ->
                    Nothing

                tower :: hs ->
                    if tower.position == point then
                        Just tower

                    else
                        checkInspectTower point hs
    in
    case gameArea of
        PlayArea ->
            case model.screen of
                PlayScreen ->
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
                            { model
                                | clicked = Just point
                                , inspectingTower = checkInspectTower point model.towers
                            }

                        ( Just point, Just placingTower ) ->
                            if placingTower.canBePlaced then
                                { model
                                    | clicked = Just point
                                    , placingTower = Nothing
                                    , towers = placingTower.tower :: model.towers
                                    , money = model.money - placingTower.tower.price
                                }

                            else
                                { model | clicked = Just point }
                    , Cmd.none
                    )

                StartScreen ->
                    case
                        mPixel
                            |> Maybe.map Area.pixelToField
                            |> Maybe.map (\(Field point) -> point)
                    of
                        Nothing ->
                            ( { model | clicked = Nothing }, Cmd.none )

                        Just point ->
                            if Button.isClicked StartScreen.startButton point then
                                ( { model | clicked = Just point, screen = PlayScreen }, Cmd.none )

                            else
                                ( { model | clicked = Just point }, Cmd.none )

                PauseScreen ->
                    ( model, Cmd.none )

                WonScreen ->
                    ( model, Cmd.none )

                LostScreen ->
                    ( model, Cmd.none )

        ToolArea ->
            ( case mPixel of
                Nothing ->
                    { model | clicked = Nothing }

                Just pixel ->
                    { model
                        | clicked = Nothing
                        , placingTower =
                            pixelToTower pixel
                                |> Maybe.map (\tower -> PlacingTower (Tower.toTower tower) False)
                    }
            , Cmd.none
            )
