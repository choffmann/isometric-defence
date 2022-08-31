module Utils.Commands exposing (generateRandomDirection, playAreaCanvas, toolAreaCanvas)

import Area exposing (Field(..))
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


playAreaCanvas : Cmd Msg
playAreaCanvas =
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


toolAreaCanvas : Cmd Msg
toolAreaCanvas =
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


checkDirection : PathPoint -> List PathDirection
checkDirection { direction, point } =
    let
        checkUp (Field { y }) =
            (y - 2) > 0

        checkDown (Field { y }) =
            (y + 2) < Area.heightTiles - 1
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


generateRandomDirection : PathPoint -> Cmd Msg
generateRandomDirection pathPoint =
    pathPoint
        |> checkDirection
        |> Path.directionGenerator
        |> Random.generate PathDirectionGenerate
