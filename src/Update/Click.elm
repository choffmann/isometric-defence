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
    case p of
        Nothing ->
            ( { model | clicked = p }, Cmd.none )

        Just coord ->
            ( { model
                | clicked =
                    coord
                        |> Pixel
                        |> pixelToField
                        |> fieldToPoint
                        |> Just
              }
            , Cmd.none
            )
