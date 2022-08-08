module Update.GeneratePath exposing (..)

import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (Path, PathDirection(..), PathPoint, pointGenerator)
import Point exposing (Point)
import Random


createNeighbourPoint : Point -> PathDirection -> Path
createNeighbourPoint point direction =
    case direction of
        Up ->
            [ PathPoint point, PathPoint (Point point.x (point.y + 1)) ]

        Down ->
            [ PathPoint point, PathPoint (Point point.x (point.y - 1)) ]

        Right ->
            [ PathPoint point, PathPoint (Point (point.x + 1) point.y) ]


setPoint : PathPoint -> Path
setPoint { point } =
    createNeighbourPoint point Right


update : Model -> ( Model, Cmd Msg )
update model =
    case model.gameState of
        Running ->
            ( model, Cmd.none )

        Paused ->
            ( model, Cmd.none )

        Won ->
            ( model, Cmd.none )

        Lost ->
            ( model, Cmd.none )

        Model.GeneratePath ->
            case model.path of
                [] ->
                    ( model, Random.generate PathPointGenerate pointGenerator )

                x :: xs ->
                    Debug.todo ""
