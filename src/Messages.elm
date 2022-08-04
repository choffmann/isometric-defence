module Messages exposing (Key(..), Msg(..))

import Browser.Dom exposing (Element)
import Point exposing (Point)


type Key
    = Space
    | F
    | UnknownKey


type Msg
    = Tick Float
    | Key Key
    | Click Point
    | Canvas (Maybe Element)
    | FullScreenChange Bool
    | EnterCanvas
