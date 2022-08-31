module Update.PathPointGenerate exposing (update)

import Area exposing (Field(..))
import Messages exposing (Msg)
import Model exposing (GameState(..), Model)
import Path exposing (PathDirection(..), PathPoint)
import Point exposing (Point)
import Utils.Commands as Commands


update : Field -> Model -> ( Model, Cmd Msg )
update (Field point) model =
    ( { model
        | path =
            Just
                [ PathPoint
                    (Point (point.x + 2) point.y
                        |> Field
                    )
                    Right
                , PathPoint
                    (Point (point.x + 1) point.y
                        |> Field
                    )
                    Right
                , PathPoint (Field point) Right
                ]
        , gameState = GeneratePath
      }
    , Commands.generateRandomDirection
        (PathPoint
            (Point (point.x + 2) point.y
                |> Field
            )
            Right
        )
    )
