module Path exposing (Path(..), PathDirection(..), PathPoint, addPathPoint, concatPath, directionGenerator, distanceToPathPoint, distanceToPixel, pointGenerator)

import Area exposing (Field(..))
import Pixel exposing (Pixel(..))
import Point exposing (Point)
import Random


type alias PathPoint =
    { point : Point, direction : PathDirection }


type Path
    = Last PathPoint (List PathPoint)


type PathDirection
    = Up
    | Down
    | Right


concatPath : Path -> Path -> Path
concatPath (Last prevPoint1 path1) (Last prevPoint2 path2) =
    Last prevPoint1 (prevPoint2 :: (path1 ++ path2))


addPathPoint : PathPoint -> Path -> Path
addPathPoint point (Last prevPoint path) =
    let
        _ =
            Debug.log "Path" (Debug.toString (Last prevPoint path))
    in
    Last point (prevPoint :: path)


pointGenerator : Random.Generator Point
pointGenerator =
    Random.map (Point 0) (Random.int 0 ((Area.area.height // Area.fieldSize) - 1))


directionGenerator : List PathDirection -> Random.Generator PathDirection
directionGenerator list =
    case list of
        [] ->
            Random.uniform Right [ Up, Down ]

        x :: xs ->
            Random.uniform x xs


distanceToPathPoint : Path -> Float -> Field
distanceToPathPoint (Last _ path) distance =
    if distance < 0 then
        Field { x = -9999, y = -9999 }

    else
        case List.drop (round (distance / toFloat Area.fieldSize)) path |> List.head of
            Nothing ->
                Field { x = 9999, y = 9999 }

            Just { point } ->
                Field point


distanceToPixel : Path -> Float -> Maybe Pixel
distanceToPixel (Last _ path) distance =
    let
        getListPoint indexRatio =
            List.drop (floor indexRatio) path
                |> List.head
                |> Maybe.map
                    (\pathPoint ->
                        let
                            tuplePositionToPixel ( newX, newY ) =
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
                                generateValue pathPoint.point.x pathPoint.point.y (+)
                                    |> tuplePositionToPixel

                            Down ->
                                generateValue pathPoint.point.y pathPoint.point.x (+)
                                    |> tuplePositionToPixel

                            Up ->
                                generateValue pathPoint.point.y pathPoint.point.x (-)
                                    |> tuplePositionToPixel
                    )
    in
    if distance < 0 then
        Nothing

    else
        getListPoint (distance / toFloat Area.fieldSize)
