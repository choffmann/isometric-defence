module Styling exposing (canvasContainerStyles, canvasStyles)

import Area exposing (area)
import Html exposing (Attribute)
import Html.Attributes exposing (id, style)


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
    , id "elm"
    , style "background-color" "white"
    ]


canvasStyles : List (Attribute msg)
canvasStyles =
    [ style "width" (toPixelString (area.width + 2 * borderWidth))
    , style "height" (toPixelString (area.height + 2 * borderWidth))
    , style "border" (toPixelString borderWidth ++ " solid black")
    ]
