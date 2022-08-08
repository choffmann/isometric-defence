module Path exposing (..)

import Area exposing (Field(..), area, fieldSize)
import List.Nonempty exposing (Nonempty, fromList, head, tail, toList)
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Random


type alias PathPoint =
    { point : Point, direction : PathDirection }


type alias Path =
    Nonempty PathPoint


type PathDirection
    = Up
    | Down
    | Right


pointGenerator : Random.Generator Point
pointGenerator =
    Random.map (Point 0) (Random.int 0 ((area.height // fieldSize) - 1))


directionGenerator : Nonempty PathDirection -> Random.Generator PathDirection
directionGenerator list =
    Random.uniform (head list) (tail list)


testPath : Maybe Path
testPath =
    fromList
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
    List.length (toList path) * fieldSize


distanceToPathPoint : Path -> Int -> Field
distanceToPathPoint path distance =
    case List.drop (distance // fieldSize) (toList path) |> List.head of
        Nothing ->
            Field { x = 0, y = 0 }

        Just { point } ->
            Field point


distanceToPixel : Path -> Int -> Pixel
distanceToPixel path distance =
    let
        getListPoint indexRatio =
            case List.drop (ceiling indexRatio) (toList path) |> List.head of
                Nothing ->
                    Pixel { x = 0, y = 0 }

                Just { point, direction } ->
                    case direction of
                        Right ->
                            Pixel { x = point.x * fieldSize + ceiling ((indexRatio - toFloat (ceiling indexRatio)) * toFloat fieldSize), y = point.y * fieldSize + ceiling (toFloat fieldSize * 0.5) }

                        Down ->
                            Pixel { x = point.x * fieldSize + ceiling (toFloat fieldSize * 0.5), y = point.y * fieldSize + ceiling ((indexRatio - toFloat (ceiling indexRatio)) * toFloat fieldSize) }

                        Up ->
                            Pixel { x = point.x * fieldSize + ceiling (toFloat fieldSize * 0.5), y = point.y * fieldSize - ceiling ((indexRatio - toFloat (ceiling indexRatio)) * toFloat fieldSize) }
    in
    getListPoint (toFloat distance / toFloat fieldSize)
