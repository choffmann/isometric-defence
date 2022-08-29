module Update.GeneratePath exposing (update)

import Area
import List.Extra as List
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (PathDirection(..), PathPoint)
import Point exposing (Point)
import Utils.Commands as Commands


update : PathDirection -> Model -> ( Model, Cmd Messages.Msg )
update direction model =
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
                            if h.point.x + 1 >= Area.widthTiles - 1 then
                                if h.point.x >= Area.widthTiles - 1 then
                                    ( { model | gameState = WaitToStart, path = Just (List.reverse ({ h | direction = direction } :: hs)) }, Cmd.none )

                                else
                                    ( { model
                                        | gameState = WaitToStart
                                        , path =
                                            Just
                                                (List.reverse
                                                    (PathPoint (Point (h.point.x + 1) h.point.y) direction
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
                                            (PathPoint (Point (h.point.x + 2) h.point.y) direction
                                                :: PathPoint (Point (h.point.x + 1) h.point.y) direction
                                                :: { h | direction = direction }
                                                :: hs
                                            )
                                  }
                                , Commands.generateRandomDirection (PathPoint (Point (h.point.x + 2) h.point.y) direction)
                                )

                        Up ->
                            ( { model
                                | path =
                                    Just
                                        (PathPoint (Point h.point.x (h.point.y - 2)) direction
                                            :: PathPoint (Point h.point.x (h.point.y - 1)) direction
                                            :: { h | direction = direction }
                                            :: hs
                                        )
                              }
                            , Commands.generateRandomDirection (PathPoint (Point h.point.x (h.point.y - 2)) direction)
                            )

                        Down ->
                            ( { model
                                | path =
                                    Just
                                        (PathPoint (Point h.point.x (h.point.y + 2)) direction
                                            :: PathPoint (Point h.point.x (h.point.y + 1)) direction
                                            :: { h | direction = direction }
                                            :: hs
                                        )
                              }
                            , Commands.generateRandomDirection (PathPoint (Point h.point.x (h.point.y + 2)) direction)
                            )
