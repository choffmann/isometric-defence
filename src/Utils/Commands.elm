module Utils.Commands exposing (getPlayAreaCanvas, getToolAreaCanvas)

import Browser.Dom exposing (Element)
import Messages exposing (GameArea(..), Msg)
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


getPlayAreaCanvas : Cmd Msg
getPlayAreaCanvas =
    Task.attempt
        (\result ->
            Messages.Canvas PlayArea
                (case result of
                    Err _ ->
                        Nothing

                    Ok element ->
                        Just (correctToCanvas element)
                )
        )
        (Browser.Dom.getElement "playAreaContainer")


getToolAreaCanvas : Cmd Msg
getToolAreaCanvas =
    Task.attempt
        (\result ->
            Messages.Canvas ToolArea
                (case result of
                    Err _ ->
                        Nothing

                    Ok element ->
                        Just (correctToCanvas element)
                )
        )
        (Browser.Dom.getElement "toolAreaContainer")
