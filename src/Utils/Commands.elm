module Utils.Commands exposing (getCanvas)

import Browser.Dom exposing (Element)
import Messages exposing (Msg)
import Styles
import Task


correctToCanvas : Element -> Element
correctToCanvas e =
    let
        correctElement element =
            { element
                | x = element.x + toFloat Styles.borderWidth
                , y = element.y + toFloat Styles.borderWidth
                , width = element.width - (2 * toFloat Styles.borderWidth)
                , height = element.height - (2 * toFloat Styles.borderWidth)
            }
    in
    { e | element = correctElement e.element }


getCanvas : Cmd Msg
getCanvas =
    Task.attempt
        (\result ->
            Messages.Canvas
                (case result of
                    Err _ ->
                        Nothing

                    Ok element ->
                        Just (correctToCanvas element)
                )
        )
        (Browser.Dom.getElement "canvasContainer")
