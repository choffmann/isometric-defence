module Path exposing (..)

import Area exposing (Field(..), fieldSize)
import Point exposing (Point)


type alias PathPoint =
    { point : Point }


type alias Path =
    List PathPoint



{- Test Pfad generierung

   type PathDirection
         = Up
         | Down
         | Right

      setPathPoint : Point -> PathDirection -> Path
        setPathPoint point direction =
            case direction of
                Up ->
                    [ PathPoint point, PathPoint (Point point.x (point.y + 1)) ]

                Down ->
                    [ PathPoint point, PathPoint (Point point.x (point.y - 1)) ]

                Right ->
                    [ PathPoint point, PathPoint (Point (point.x + 1) point.y) ]
-}


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


pathLength : Path -> Int
pathLength path =
    List.length path * fieldSize


distanceToPathPoint : Path -> Int -> Field
distanceToPathPoint path distance =
    case List.drop (distance // pathLength path) path |> List.head of
        Nothing ->
            Field { x = 0, y = 0 }

        Just { point } ->
            Field point
