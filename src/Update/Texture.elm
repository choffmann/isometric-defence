module Update.Texture exposing (update)

import Area
import Canvas.Texture as Texture exposing (Texture)
import Messages exposing (Msg)
import Model exposing (Model)
import Utils.Data exposing (Load(..))


sprite : Float -> Float -> Float -> Float -> Texture -> Texture
sprite x y w h =
    Texture.sprite
        { x = x * w
        , y = y * h
        , width = w
        , height = h
        }


update : Maybe Texture -> Model -> ( Model, Cmd Msg )
update maybeTexture model =
    case maybeTexture of
        Nothing ->
            ( { model | sprite = Failure }, Cmd.none )

        Just texture ->
            let
                towerAreaWidth =
                    64

                towerAreaHeight =
                    64

                gameViewWidth =
                    toFloat Area.fieldSize

                gameViewHeight =
                    toFloat Area.fieldSize
            in
            ( { model
                | sprite =
                    Success
                        { gameView =
                            { topDown =
                                { enemy =
                                    { cardBoardBox = sprite 2 5 gameViewWidth gameViewHeight texture
                                    , woodBox = sprite 1 5 gameViewWidth gameViewHeight texture
                                    , redBox = sprite 1 6 gameViewWidth gameViewHeight texture
                                    , yellowBox = sprite 3 5 gameViewWidth gameViewHeight texture
                                    , blueBox = sprite 0 6 gameViewWidth gameViewHeight texture
                                    , metalBox = sprite 2 6 gameViewWidth gameViewHeight texture
                                    , palette =
                                        { state1 = sprite 0 7 gameViewWidth gameViewHeight texture
                                        , state2 = sprite 1 7 gameViewWidth gameViewHeight texture
                                        , state3 = sprite 2 7 gameViewWidth gameViewHeight texture
                                        }
                                    }
                                }
                            , isometric =
                                { floor = sprite 0 0 gameViewWidth gameViewHeight texture
                                , path = sprite 1 0 gameViewWidth gameViewHeight texture
                                , towerCanNotPlaced = sprite 2 3 gameViewWidth gameViewHeight texture
                                , towers =
                                    { basic =
                                        { tower = sprite 0 1 gameViewWidth gameViewHeight texture
                                        , selection = sprite 1 1 gameViewWidth gameViewHeight texture
                                        }
                                    , tower1 =
                                        { tower = sprite 2 1 gameViewWidth gameViewHeight texture
                                        , selection = sprite 3 1 gameViewWidth gameViewHeight texture
                                        }
                                    , tower2 =
                                        { tower = sprite 0 2 gameViewWidth gameViewHeight texture
                                        , selection = sprite 1 2 gameViewWidth gameViewHeight texture
                                        }
                                    , tower3 =
                                        { tower = sprite 2 2 gameViewWidth gameViewHeight texture
                                        , selection = sprite 3 2 gameViewWidth gameViewHeight texture
                                        }
                                    , tower4 =
                                        { tower = sprite 0 3 gameViewWidth gameViewHeight texture
                                        , selection = sprite 1 3 gameViewWidth gameViewHeight texture
                                        }
                                    }
                                , enemy =
                                    { cardBoardBox = sprite 0 4 gameViewWidth gameViewHeight texture
                                    , woodBox = sprite 3 3 gameViewWidth gameViewHeight texture
                                    , redBox = sprite 1 4 gameViewWidth gameViewHeight texture
                                    , yellowBox = sprite 2 4 gameViewWidth gameViewHeight texture
                                    , blueBox = sprite 3 4 gameViewWidth gameViewHeight texture
                                    , metalBox = sprite 0 5 gameViewWidth gameViewHeight texture
                                    , palette =
                                        { state1 = sprite 0 7 gameViewWidth gameViewHeight texture
                                        , state2 = sprite 1 7 gameViewWidth gameViewHeight texture
                                        , state3 = sprite 2 7 gameViewWidth gameViewHeight texture
                                        }
                                    }
                                }
                            }
                        , towerArea =
                            { deselect = sprite 2 3 towerAreaWidth towerAreaHeight texture
                            , basic = sprite 2 0 towerAreaWidth towerAreaHeight texture
                            , tower1 = sprite 2 1 towerAreaWidth towerAreaHeight texture
                            , tower2 = sprite 2 2 towerAreaWidth towerAreaHeight texture
                            , tower3 = sprite 2 3 towerAreaWidth towerAreaHeight texture
                            , tower4 = sprite 2 4 towerAreaWidth towerAreaHeight texture
                            }
                        , ui =
                            { coin = sprite 2 0 gameViewWidth gameViewHeight texture
                            , heart = sprite 3 0 gameViewWidth gameViewHeight texture
                            , buttons =
                                { start = sprite 0 9 128 32 texture }
                            }
                        }
              }
            , Cmd.none
            )
