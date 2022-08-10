module Path exposing (Path, PathDirection(..), PathPoint, directionGenerator, distanceToPathPoint, distanceToPixel, pointGenerator)

import Area exposing (Field(..), area, fieldSize)
import List.Nonempty as Nonempty exposing (Nonempty)
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
    Random.uniform (Nonempty.head list) (Nonempty.tail list)


distanceToPathPoint : Path -> Float -> Field
distanceToPathPoint path distance =
    if distance < 0 then
        Field { x = -9999, y = -9999 }

    else
        case List.drop (round (distance / toFloat fieldSize)) (Nonempty.toList path) |> List.head of
            Nothing ->
                Field { x = 9999, y = 9999 }

            Just { point } ->
                Field point


distanceToPixel : Path -> Float -> Maybe Pixel
distanceToPixel path distance =
    let
        getListPoint indexRatio =
            List.drop (floor indexRatio) (Nonempty.toList path)
                |> List.head
                |> Maybe.map
                    (\pathPoint ->
                        let
                            generateValue main second op =
                                ( floor (toFloat fieldSize * 0.5) + floor ((indexRatio - toFloat (floor indexRatio)) * toFloat fieldSize) |> op (main * fieldSize)
                                , second * fieldSize + floor (toFloat fieldSize * 0.5)
                                )
                        in
                        case pathPoint.direction of
                            Right ->
                                case generateValue pathPoint.point.x pathPoint.point.y (+) of
                                    ( newX, newY ) ->
                                        Pixel { x = newX, y = newY }

                            Down ->
                                case generateValue pathPoint.point.y pathPoint.point.x (+) of
                                    ( newY, newX ) ->
                                        Pixel { x = newX, y = newY }

                            Up ->
                                case generateValue pathPoint.point.y pathPoint.point.x (-) of
                                    ( newY, newX ) ->
                                        Pixel { x = newX, y = newY }
                    )
    in
    if distance < 0 then
        Nothing

    else
        getListPoint (distance / toFloat fieldSize)
