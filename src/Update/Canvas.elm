module Update.Canvas exposing (update)

import Browser.Dom exposing (Element)
import Messages exposing (GameArea(..))
import Model exposing (Model)


update : GameArea -> Maybe Element -> Model -> ( Model, Cmd msg )
update gameArea element model =
    ( case gameArea of
        PlayArea ->
            { model
                | playCanvas =
                    element
            }

        ToolArea ->
            { model
                | toolCanvas =
                    element
            }
    , Cmd.none
    )
