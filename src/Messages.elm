module Messages exposing (Key(..), Msg(..), ReceivingEvents(..), SendingEvents(..))

import Browser.Dom exposing (Element)
import FullScreenMode exposing (FullScreenMode)
import Point exposing (Point)


type Key
    = Space
    | F
    | R
    | ArrowDown
    | ArrowUp
    | UnknownKey


type ReceivingEvents
    = FullScreenChanged FullScreenMode
    | UnknownEvent


type SendingEvents
    = ChangeFullScreen FullScreenMode


type Msg
    = Tick Float
    | Key Key
    | Click (Maybe Point)
    | Canvas (Maybe Element)
    | EnterCanvas
    | Event ReceivingEvents
