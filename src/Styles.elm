module Styles exposing (appContainer, borderWidth, canvasContainerStyles, canvasStyles)

import Area
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


canvasStyles : List (Attribute msg)
canvasStyles =
    [ style "width" (toPixelString (Area.area.width + 2 * borderWidth))
    , style "height" (toPixelString (Area.area.height + 2 * borderWidth))
    , style "border" (toPixelString borderWidth ++ " solid black")
    ]


appContainer : List (Attribute msg)
appContainer =
    [ style "background-color" "white" ]
