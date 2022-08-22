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
                        , tower =
                            { selectTower =
                                { towerCanPlaced = sprite 0 1 texture
                                , towerCanNotPlaced = sprite 1 1 texture
                                }
                            , towers =
                                { basic = sprite 2 0 texture
                                , tower1 = sprite 2 0 texture
                                }
                            }
                        , enemy = sprite 1 2 texture
                        }
              }
            , Cmd.none
            )
