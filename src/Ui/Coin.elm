module Ui.Coin exposing (..)

import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Point exposing (Point)
import Ui.DrawUtils as DrawUtils


drawCoin : Point -> Texture -> Renderable
drawCoin point texture =
    Canvas.texture [] (DrawUtils.convertToCanvasPoint point) texture
