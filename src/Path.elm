module Path exposing (Path, PathDirection(..), PathPoint, addPathPoint, directionGenerator, distanceToPathPoint, distanceToPixel, pointGenerator)

import Area exposing (Field(..), Pixel(..), heightTiles)
import Point exposing (Point)
import Random


type alias PathPoint =
    { point : Field, direction : PathDirection }


type alias Path =
    List PathPoint


type PathDirection
    = Up
    | Down
    | Right


addPathPoint : PathPoint -> Path -> Path
addPathPoint point path =
    path ++ [ point ]


pointGenerator : Random.Generator Field
pointGenerator =
    Random.map (Point 0) (Random.int 0 (heightTiles - 1))
        |> Random.map Field


directionGenerator : List PathDirection -> Random.Generator PathDirection
directionGenerator list =
    case list of
        [] ->
            Random.uniform Right [ Up, Down ]

        x :: xs ->
            Random.uniform x xs


distanceToFieldAndRatio : Path -> Float -> ( PathPoint, Float )
distanceToFieldAndRatio path distance =
    let
        getListPoint indexRatio =
            case
                List.drop (floor indexRatio) path
                    |> List.head
            of
                Nothing ->
                    ( PathPoint (Field (Point 9999 9999)) Right, -1 )

                Just pathPoint ->
                    ( pathPoint, indexRatio - toFloat (floor indexRatio) )
    in
    if distance < 0 then
        ( PathPoint (Field (Point -9999 -9999)) Right, -1 )

    else
        getListPoint (distance / toFloat Area.fieldSize)


distanceToPathPoint : Path -> Float -> Field
distanceToPathPoint path distance =
    let
        internal ( { point }, _ ) =
            point
    in
    distanceToFieldAndRatio path distance
        |> internal


distanceToPixel : Path -> Float -> Maybe Pixel
distanceToPixel path distance =
    let
        internal ( pathPoint, ratio ) =
            let
                generateRightValue (Field { x, y }) =
                    Point
                        (x * Area.fieldSize + floor (ratio * toFloat Area.fieldSize))
                        (y * Area.fieldSize)
                        |> Pixel

                generateDownValue (Field { x, y }) =
                    Point
                        (x * Area.fieldSize)
                        (y * Area.fieldSize + floor (ratio * toFloat Area.fieldSize))
                        |> Pixel

                generateUpValue (Field { x, y }) =
                    Point
                        (x * Area.fieldSize)
                        (y * Area.fieldSize - floor (ratio * toFloat Area.fieldSize))
                        |> Pixel
            in
            if ratio < 0 then
                Nothing

            else
                Just
                    (case pathPoint.direction of
                        Right ->
                            pathPoint.point
                                |> generateRightValue

                        Down ->
                            pathPoint.point
                                |> generateDownValue

                        Up ->
                            pathPoint.point
                                |> generateUpValue
                    )
    in
    distanceToFieldAndRatio path distance
        |> internal
