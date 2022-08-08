module Update.Canvas exposing (update)

import Browser.Dom exposing (Element)
import Model exposing (Model)


update : Maybe Element -> Model -> ( Model, Cmd msg )
update element model =
    ( { model
        | canvas =
            element
      }
    , Cmd.none
    )
