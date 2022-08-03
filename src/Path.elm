module Path exposing (..)

import Point exposing (Point)


type alias PathPoint =
    { point : Point }


type alias Path =
    List PathPoint


testPath : Path
testPath =
    [ PathPoint (Point 0 0)
    , PathPoint (Point 1 0)
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
    , PathPoint (Point 10 10)
    ]
