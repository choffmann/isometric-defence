module Messages exposing (GameArea(..), Key(..), Msg(..), ReceivingEvents(..), SendingEvents(..))

import Area exposing (Field, Pixel)
import Browser.Dom exposing (Element)
import Canvas.Texture as Canvas
import FullScreenMode exposing (FullScreenMode)
import Path exposing (PathDirection)


type Key
    = Space
    | F
    | R
    | I
    | P
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
    | PathPointGenerate Field
    | TextureLoaded (Maybe Canvas.Texture)
