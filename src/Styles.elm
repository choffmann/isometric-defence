module Styles exposing (appContainer, borderWidth, canvasContainerStyles, canvasStyles)

import Area exposing (Area)
import GameView exposing (GameView(..))
import Html exposing (Attribute)
import Html.Attributes exposing (style)


borderWidth : Int
borderWidth =
    10


toPixelString : Int -> String
toPixelString pixel =
    String.fromInt pixel ++ "px"


canvasContainerStyles : List (Attribute msg)
canvasContainerStyles =
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]


canvasStyles : Area -> GameView -> List (Attribute msg)
canvasStyles area gameView =
    case gameView of
        TopDown ->
            [ style "width" (toPixelString (area.width + 2 * borderWidth))
            , style "height" (toPixelString (area.height + 2 * borderWidth))
            , style "border" (toPixelString borderWidth ++ " solid black")
            ]

        Isometric ->
            [ style "width" (toPixelString (area.width + 2 * borderWidth))
            , style "height" (toPixelString ((area.height // 2) + 2 * borderWidth))
            , style "border" (toPixelString borderWidth ++ " solid black")
            ]


appContainer : List (Attribute msg)
appContainer =
    [ style "background-color" "white" ]
