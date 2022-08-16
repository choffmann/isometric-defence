module Update.Click exposing (update)

import Area exposing (Field(..), pixelToField)
import Model exposing (Model)
import Pixel exposing (Pixel(..))
import Point exposing (Point)


update : Maybe Point -> Model -> ( Model, Cmd msg )
update p model =
    let
        fieldToPoint (Field point) =
            point
    in
    ( { model
        | clicked =
            p
                |> Maybe.map
                    (\coord ->
                        coord
                            |> Pixel
                            |> pixelToField
                            |> fieldToPoint
                    )
      }
    , Cmd.none
    )
