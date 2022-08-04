module Utils.Commands exposing (getCanvas)

import Browser.Dom exposing (getElement)
import Messages exposing (Msg)
import Task


getCanvas : Cmd Msg
getCanvas =
    Task.attempt
        (\result ->
            Messages.Canvas
                (case Debug.log "result" result of
                    Err _ ->
                        Nothing

                    Ok element ->
                        Just element
                )
        )
        (getElement "elm")
