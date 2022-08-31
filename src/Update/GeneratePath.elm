module Update.GeneratePath exposing (update)

import Area exposing (Field(..))
import Messages exposing (Msg)
import Model exposing (GameState(..), Model)
import Path exposing (PathDirection(..), PathPoint)
import Point exposing (Point)
import Utils.Commands as Commands


update : PathDirection -> Model -> ( Model, Cmd Msg )
update direction model =
    let
        oldFieldToNewField (Field { x, y }) offsetX offsetY =
            Point (x + offsetX) (y + offsetY)
                |> Field
    in
    case model.path of
        Nothing ->
            ( model, Cmd.none )

        Just path ->
            case path of
                [] ->
                    ( model, Cmd.none )

                h :: hs ->
                    case direction of
                        Right ->
                            if (Area.fieldToPoint h.point).x + 1 >= Area.widthTiles - 1 then
                                if (Area.fieldToPoint h.point).x >= Area.widthTiles - 1 then
                                    ( { model | gameState = WaitToStart, path = Just (List.reverse ({ h | direction = direction } :: hs)) }, Cmd.none )

                                else
                                    ( { model
                                        | gameState = WaitToStart
                                        , path =
                                            Just
                                                (List.reverse
                                                    (PathPoint (oldFieldToNewField h.point 1 0) direction
                                                        :: { h | direction = direction }
                                                        :: hs
                                                    )
                                                )
                                      }
                                    , Cmd.none
                                    )

                            else
                                ( { model
                                    | path =
                                        Just
                                            (PathPoint (oldFieldToNewField h.point 2 0) direction
                                                :: PathPoint (oldFieldToNewField h.point 1 0) direction
                                                :: { h | direction = direction }
                                                :: hs
                                            )
                                  }
                                , Commands.generateRandomDirection (PathPoint (oldFieldToNewField h.point 2 0) direction)
                                )

                        Up ->
                            ( { model
                                | path =
                                    Just
                                        (PathPoint (oldFieldToNewField h.point 0 -2) direction
                                            :: PathPoint (oldFieldToNewField h.point 0 -1) direction
                                            :: { h | direction = direction }
                                            :: hs
                                        )
                              }
                            , Commands.generateRandomDirection (PathPoint (oldFieldToNewField h.point 0 -2) direction)
                            )

                        Down ->
                            ( { model
                                | path =
                                    Just
                                        (PathPoint (oldFieldToNewField h.point 0 2) direction
                                            :: PathPoint (oldFieldToNewField h.point 0 1) direction
                                            :: { h | direction = direction }
                                            :: hs
                                        )
                              }
                            , Commands.generateRandomDirection (PathPoint (oldFieldToNewField h.point 0 2) direction)
                            )
