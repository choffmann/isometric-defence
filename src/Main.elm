module Main exposing (main)

import Browser
import Html exposing (Html, text)


type Msg
    = Msg


type alias Model =
    { msg : String }


type alias Flags =
    { msg : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { msg = flags.msg }, Cmd.none )


view : Model -> Html Msg
view model =
    text model.msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
