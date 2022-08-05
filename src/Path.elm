module Path exposing (..)

import Area exposing (Area)
import Point exposing (Point)
import Random


pathSize : Int
pathSize =
    30


type alias PathPoint =
    { point : Point }


type alias Path =
    List PathPoint


type PathDirection
    = Up
    | Down
    | Right


startPointGenerator : Area -> Random.Generator Point
startPointGenerator area =
    Random.map (Point 0) (Random.int 0 area.width)


createPath : Area -> Path -> Path
createPath area path =
    case path of
        [] ->
            createPath area (setPathPoint (Point 0 4) Right)

        x :: xs ->
            []


setPathPoint : Point -> PathDirection -> Path
setPathPoint point direction =
    case direction of
        Up ->
            [ PathPoint point, PathPoint (Point point.x (point.y + 1)) ]

        Down ->
            [ PathPoint point, PathPoint (Point point.x (point.y - 1)) ]

        Right ->
            [ PathPoint point, PathPoint (Point (point.x + 1) point.y) ]


testPath : Path
testPath =
    [ PathPoint (Point 0 1)
    , PathPoint (Point 1 1)
    , PathPoint (Point 1 2)
    , PathPoint (Point 1 3)
    , PathPoint (Point 1 4)
    , PathPoint (Point 1 5)
    , PathPoint (Point 1 6)
    , PathPoint (Point 2 6)
    , PathPoint (Point 3 6)
    , PathPoint (Point 4 6)
    , PathPoint (Point 5 6)
    , PathPoint (Point 6 6)
    , PathPoint (Point 7 6)
    , PathPoint (Point 7 7)
    , PathPoint (Point 7 8)
    , PathPoint (Point 7 9)
    , PathPoint (Point 8 9)
    , PathPoint (Point 9 9)
    ]
