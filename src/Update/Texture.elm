module Update.Texture exposing (..)

import Area
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
            let
                sprite : Float -> Float -> Texture -> Texture
                sprite x y spriteTexture =
                    Texture.sprite
                        { x = x * toFloat Area.fieldSize
                        , y = y * toFloat Area.fieldSize
                        , width = toFloat Area.fieldSize
                        , height = toFloat Area.fieldSize
                        }
                        spriteTexture
            in
            ( { model
                | sprites =
                    Success
                        { floor = sprite 0 0 texture
                        , path = sprite 1 0 texture
                        , towerCanNotPlaced = sprite 0 3 texture
                        , towers =
                            { basic =
                                { tower = sprite 0 1 texture
                                , selection = sprite 1 1 texture
                                }
                            , tower1 =
                                { tower = sprite 2 1 texture
                                , selection = sprite 3 1 texture
                                }
                            , tower2 =
                                { tower = sprite 0 2 texture
                                , selection = sprite 1 2 texture
                                }
                            , tower3 =
                                { tower = sprite 2 2 texture
                                , selection = sprite 3 2 texture
                                }
                            }
                        , enemy = sprite 2 1 texture
                        }
              }
            , Cmd.none
            )
