module Messages exposing (Key(..), MouseButton(..), MouseClick, Msg(..), ReceivingEvents(..), SendingEvents(..))

import Browser.Dom exposing (Element)
import FullScreenMode exposing (FullScreenMode)
import Path exposing (PathDirection)
import Point exposing (Point)


type Key
    = Space
    | F
    | R
    | ArrowDown
    | ArrowUp
    | UnknownKey


type MouseButton
    = Left
    | Right
    | UnknownButton


type alias MouseClick =
    { point : Maybe Point, button : MouseButton }


type ReceivingEvents
    = FullScreenChanged FullScreenMode
    | UnknownEvent


type SendingEvents
    = ChangeFullScreen FullScreenMode


type Msg
    = Tick Float
    | Key Key
    | LeftClick (Maybe Point)
    | RightClick
    | MovePosition (Maybe Point)
    | Canvas (Maybe Element)
    | EnterCanvas
    | Event ReceivingEvents
    | PathDirectionGenerate PathDirection
    | PathPointGenerate Point
