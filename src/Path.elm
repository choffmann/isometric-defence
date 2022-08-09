module Path exposing (..)

import Area exposing (Field(..), fieldSize)
import Pixel exposing (Pixel(..))
import Point exposing (Point)


type PathDirection
    = Up
    | Down
    | Right


type alias PathPoint =
    { point : Point, direction : PathDirection }


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
    [ PathPoint (Point 0 1) Right
    , PathPoint (Point 1 1) Down
    , PathPoint (Point 1 2) Down
    , PathPoint (Point 1 3) Down
    , PathPoint (Point 1 4) Down
    , PathPoint (Point 1 5) Down
    , PathPoint (Point 1 6) Right
    , PathPoint (Point 2 6) Right
    , PathPoint (Point 3 6) Right
    , PathPoint (Point 4 6) Right
    , PathPoint (Point 5 6) Right
    , PathPoint (Point 6 6) Right
    , PathPoint (Point 7 6) Down
    , PathPoint (Point 7 7) Down
    , PathPoint (Point 7 8) Down
    , PathPoint (Point 7 9) Right
    , PathPoint (Point 8 9) Right
    , PathPoint (Point 9 9) Right
    ]


pathLength : Path -> Int
pathLength path =
    List.length path * fieldSize


distanceToPathPoint : Path -> Float -> Field
distanceToPathPoint path distance =
    if distance < 0 then
        Field { x = -9999, y = -9999 }

    else
        case List.drop (round (distance / toFloat fieldSize)) path |> List.head of
            Nothing ->
                Field { x = 9999, y = 9999 }

            Just { point } ->
                Field point


distanceToPixel : Path -> Float -> Maybe Pixel
distanceToPixel path distance =
    let
        getListPoint indexRatio =
            List.drop (floor indexRatio) path
                |> List.head
                |> Maybe.map
                    (\pathpoint ->
                        let
                            generateValue main second op =
                                ( floor (toFloat fieldSize * 0.5) + floor ((indexRatio - toFloat (floor indexRatio)) * toFloat fieldSize) |> op (main * fieldSize)
                                , second * fieldSize + floor (toFloat fieldSize * 0.5)
                                )
                        in
                        case pathpoint.direction of
                            Right ->
                                case generateValue pathpoint.point.x pathpoint.point.y (+) of
                                    ( newX, newY ) ->
                                        Pixel { x = newX, y = newY }

                            Down ->
                                case generateValue pathpoint.point.y pathpoint.point.x (+) of
                                    ( newY, newX ) ->
                                        Pixel { x = newX, y = newY }

                            Up ->
                                case generateValue pathpoint.point.y pathpoint.point.x (-) of
                                    ( newY, newX ) ->
                                        Pixel { x = newX, y = newY }
                    )
    in
    if distance < 0 then
        Nothing

    else
        getListPoint (distance / toFloat fieldSize)
