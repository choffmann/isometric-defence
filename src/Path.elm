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


distanceToPathPoint : Path -> Float -> Field
distanceToPathPoint path distance =
    if distance < 0 then
        Field { x = -9999, y = -9999 }

    else
        case List.drop (round (distance / toFloat fieldSize)) (toList path) |> List.head of
            Nothing ->
                Field { x = 9999, y = 9999 }

            Just { point } ->
                Field point


distanceToPixel : Path -> Float -> Maybe Pixel
distanceToPixel path distance =
    let
        getListPoint indexRatio =
            List.drop (floor indexRatio) (toList path)
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
