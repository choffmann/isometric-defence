module Ui.Enemy exposing (enemiesToCanvas)

import Canvas exposing (Renderable)
import Canvas.Settings
import Color
import Enemy exposing (Enemy)
import List.Extra as List
import Path exposing (Path)
import Pixel exposing (Pixel(..))


enemiesToCanvas : List Enemy -> Maybe Path -> Renderable
enemiesToCanvas enemies path =
    case path of
        Nothing ->
            Canvas.shapes [] []

        Just justPath ->
            enemies
                |> List.map
                    (\enemy ->
                        Path.distanceToPixel justPath enemy.distance
                            |> Maybe.map
                                (\(Pixel point) ->
                                    Canvas.rect ( toFloat point.x - 10, toFloat point.y - 10 ) 20 20
                                )
                    )
                |> List.removeNothing
                |> Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 50 255 50) ]
