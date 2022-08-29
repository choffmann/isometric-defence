module Utils.Commands exposing (generateRandomDirection, getPlayAreaCanvas, getToolAreaCanvas)

import Area
import Browser.Dom exposing (Element)
import Messages exposing (GameArea(..), Msg(..))
import Path exposing (PathDirection(..), PathPoint)
import Random
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


generateRandomDirection : PathPoint -> Cmd Msg
generateRandomDirection pathPoint =
    let
        checkDirection { direction, point } =
            let
                checkUp p =
                    (p.y - 2) > 0

                checkDown p =
                    (p.y + 2) < Area.heightTiles - 1
            in
            case direction of
                Right ->
                    if checkDown point && checkUp point then
                        [ Right, Up, Down ]

                    else if checkDown point then
                        [ Right, Down ]

                    else if checkUp point then
                        [ Right, Up ]

                    else
                        [ Right ]

                Down ->
                    if checkDown point then
                        [ Right, Down ]

                    else
                        [ Right ]

                Up ->
                    if checkUp point then
                        [ Right, Up ]

                    else
                        [ Right ]
    in
    pathPoint
        |> checkDirection
        |> Path.directionGenerator
        |> Random.generate PathDirectionGenerate
