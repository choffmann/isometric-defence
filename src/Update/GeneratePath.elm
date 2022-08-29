module Update.GeneratePath exposing (update)

import Area
import List.Extra as List
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (PathDirection(..), PathPoint)
import Point exposing (Point)
import Random


checkDirection : PathPoint -> List PathDirection
checkDirection { direction, point } =
    let
        checkUp p =
            (p.y - 2) > 0

        checkDown p =
            (p.y + 2) < Area.heightTiles - 1
    in
    case direction of
        Right ->
            if checkDown point && checkUp point then
                [ Right, Up, Down ]

            else if checkDown point then
                [ Right, Down ]

            else if checkUp point then
                [ Right, Up ]

            else
                [ Right ]

        Down ->
            if checkDown point then
                [ Right, Down ]

            else
                [ Right ]

        Up ->
            if checkUp point then
                [ Right, Up ]

            else
                [ Right ]


generateRandomDirection : PathPoint -> Cmd Msg
generateRandomDirection pathPoint =
    pathPoint
        |> checkDirection
        |> Path.directionGenerator
        |> Random.generate PathDirectionGenerate


update : Msg -> Model -> ( Model, Cmd Messages.Msg )
update msg model =
    case model.gameState of
        Running ->
            ( model, Cmd.none )

        Paused ->
            ( model, Cmd.none )

        Won ->
            ( model, Cmd.none )

        Lost ->
            ( model, Cmd.none )

        WaitToStart ->
            ( model, Cmd.none )

        Model.GeneratePath ->
            case msg of
                PathDirectionGenerate direction ->
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
                                                , generateRandomDirection (PathPoint (Point (h.point.x + 2) h.point.y) direction)
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
                                            , generateRandomDirection (PathPoint (Point h.point.x (h.point.y - 2)) direction)
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
                                            , generateRandomDirection (PathPoint (Point h.point.x (h.point.y + 2)) direction)
                                            )

                PathPointGenerate point ->
                    ( { model
                        | path =
                            Just
                                [ PathPoint (Point (point.x + 2) point.y) Right
                                , PathPoint (Point (point.x + 1) point.y) Right
                                , PathPoint point Right
                                ]
                        , gameState = GeneratePath
                      }
                    , generateRandomDirection (PathPoint (Point (point.x + 2) point.y) Right)
                    )

                _ ->
                    ( model, Cmd.none )
