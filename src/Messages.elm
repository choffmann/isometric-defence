module Messages exposing (Key(..), Msg(..), ReceivingEvents(..), SendingEvents(..))

import Browser.Dom exposing (Element)
import FullScreenMode exposing (FullScreenMode)
import Path exposing (PathDirection, PathPoint)
import Point exposing (Point)


type Key
    = Space
    | F
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
    | GeneratePath
    | PathDirectionGenerate PathDirection
    | PathPointGenerate Point
