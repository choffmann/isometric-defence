module Messages exposing (GameArea(..), Key(..), Msg(..), ReceivingEvents(..), SendingEvents(..))

import Browser.Dom exposing (Element)
import Canvas.Texture as Canvas
import FullScreenMode exposing (FullScreenMode)
import Path exposing (PathDirection)
import Pixel exposing (Pixel)
import Point exposing (Point)
import Screen exposing (Screen)


type Key
    = Space
    | F
    | R
    | I
    | ArrowDown
    | ArrowUp
    | UnknownKey


type GameArea
    = PlayArea
    | ToolArea


type ReceivingEvents
    = FullScreenChanged FullScreenMode
    | UnknownEvent


type SendingEvents
    = ChangeFullScreen FullScreenMode


type Msg
    = Tick Float
    | Key Key
    | LeftClick GameArea (Maybe Pixel)
    | RightClick
    | MovePosition GameArea (Maybe Pixel)
    | Canvas GameArea (Maybe Element)
    | EnterCanvas
    | Event ReceivingEvents
    | PathDirectionGenerate PathDirection
    | PathPointGenerate Point
    | TextureLoaded (Maybe Canvas.Texture)
    | ChangeScreen Screen
