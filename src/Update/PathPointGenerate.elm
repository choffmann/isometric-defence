module Update.PathPointGenerate exposing (update)

import Messages exposing (Msg)
import Model exposing (GameState(..), Model)
import Path exposing (PathDirection(..), PathPoint)
import Point exposing (Point)
import Utils.Commands as Commands


update : Point -> Model -> ( Model, Cmd Msg )
update point model =
    ( { model
        | path =
            Just
                [ PathPoint (Point (point.x + 2) point.y) Right
                , PathPoint (Point (point.x + 1) point.y) Right
                , PathPoint point Right
                ]
        , gameState = GeneratePath
      }
    , Commands.generateRandomDirection (PathPoint (Point (point.x + 2) point.y) Right)
    )
