module Update.Texture exposing (..)

import Canvas.Texture as Texture exposing (Texture)
import Messages exposing (Msg)
import Model exposing (Model)
import Utils.Data exposing (Load(..))


update : Maybe Texture -> Model -> ( Model, Cmd Msg )
update maybeTexture model =
    case maybeTexture of
        Nothing ->
            ( { model | sprites = Failure }, Cmd.none )

        Just texture ->
            ( { model | sprites = Success { floor = Texture.sprite { x = 0, y = 0, width = 32, height = 32 } texture } }
            , Cmd.none
            )
