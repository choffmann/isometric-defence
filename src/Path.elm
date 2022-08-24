module Path exposing (Path, PathDirection(..), PathPoint, directionGenerator, distanceToPathPoint, distanceToPixel, pointGenerator)

import Area exposing (Field(..))
import List.Nonempty as Nonempty exposing (Nonempty(..))
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
    Random.map (Point 0) (Random.int 0 ((Area.area.height // Area.fieldSize) - 1))


directionGenerator : List PathDirection -> Random.Generator PathDirection
directionGenerator list =
    case list of
        [] ->
            -- TODO: Was wenn Liste leer? - Kann aber eigentlich nie sein
            Random.uniform Right [ Up, Down ]

        x :: xs ->
            Random.uniform x xs


distanceToPathPoint : Path -> Float -> Field
distanceToPathPoint path distance =
    if distance < 0 then
        Field { x = -9999, y = -9999 }

    else
        case List.drop (round (distance / toFloat Area.fieldSize)) (Nonempty.toList path) |> List.head of
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
                            tuplePositionToPixelXY ( newX, newY ) =
                                Pixel { x = newX, y = newY }

                            tuplePositionToPixelYX ( newY, newX ) =
                                Pixel { x = newX, y = newY }

                            generateValue main second op =
                                ( floor (toFloat Area.fieldSize * 0.5)
                                    + floor ((indexRatio - toFloat (floor indexRatio)) * toFloat Area.fieldSize)
                                    |> op (main * Area.fieldSize)
                                , second * Area.fieldSize + floor (toFloat Area.fieldSize * 0.5)
                                )
                        in
                        case pathPoint.direction of
                            Right ->
                                generateValue pathPoint.point.x pathPoint.point.y (+) |> tuplePositionToPixelXY

                            Down ->
                                generateValue pathPoint.point.y pathPoint.point.x (+) |> tuplePositionToPixelYX

                            Up ->
                                generateValue pathPoint.point.y pathPoint.point.x (-) |> tuplePositionToPixelYX
                    )
    in
    if distance < 0 then
        Nothing

    else
        getListPoint (distance / toFloat Area.fieldSize)
