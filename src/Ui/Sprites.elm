module Ui.Sprites exposing (..)

import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)


type alias Sprites =
    { floor : Texture }


renderFloorSprite : Texture -> Renderable
renderFloorSprite sprite =
    Canvas.texture [] ( 0, 0 ) sprite
