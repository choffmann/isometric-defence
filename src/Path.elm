module Path exposing (Path(..), PathDirection(..), PathPoint, addPathPoint, directionGenerator, distanceToPathPoint, distanceToPixel, pointGenerator, testPath)

import Area exposing (Field(..))
import List.Extra
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


testPath : Path
testPath =
    Last (PathPoint (Point 1 2) Right)
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
        , PathPoint (Point 9 9) Up
        , PathPoint (Point 9 8) Up
        , PathPoint (Point 9 7) Up
        , PathPoint (Point 9 6) Up
        , PathPoint (Point 9 5) Right
        , PathPoint (Point 10 5) Right
        , PathPoint (Point 11 5) Right
        ]


addPathPoint : PathPoint -> Path -> Path
addPathPoint point (Last prevPoint path) =
    let
        _ =
            Debug.log "Path" (Debug.toString (Last prevPoint path))
    in
    Last point (List.Extra.snoc path prevPoint)


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


distanceToFieldAndRatio : Path -> Float -> ( PathPoint, Float )
distanceToFieldAndRatio (Last _ path) distance =
    let
        getListPoint indexRatio =
            case
                List.drop (floor indexRatio) path
                    |> List.head
            of
                Nothing ->
                    ( PathPoint { x = -9999, y = -9999 } Right, -1 )

                Just pathPoint ->
                    ( pathPoint, indexRatio - toFloat (floor indexRatio) )
    in
    if distance < 0 then
        ( PathPoint { x = 9999, y = 9999 } Right, -1 )

    else
        getListPoint (distance / toFloat Area.fieldSize)


distanceToPathPoint : Path -> Float -> Field
distanceToPathPoint path distance =
    let
        internal ( { point }, _ ) =
            Field point
    in
    distanceToFieldAndRatio path distance
        |> internal


distanceToPixel : Path -> Float -> Maybe Pixel
distanceToPixel path distance =
    let
        tuplePositionToPixelXY ( newX, newY ) =
            Pixel { x = newX, y = newY }

        tuplePositionToPixelYX ( newY, newX ) =
            Pixel { x = newX, y = newY }

        internal ( pathPoint, ratio ) =
            let
                generateValue main second op =
                    ( floor (toFloat Area.fieldSize * 0.5)
                        + floor (ratio * toFloat Area.fieldSize)
                        |> op (main * Area.fieldSize)
                    , second * Area.fieldSize + floor (toFloat Area.fieldSize * 0.5)
                    )
            in
            if ratio < 0 then
                Nothing

            else
                Just
                    (case pathPoint.direction of
                        Right ->
                            generateValue pathPoint.point.x pathPoint.point.y (+) |> tuplePositionToPixelXY

                        Down ->
                            generateValue pathPoint.point.y pathPoint.point.x (+) |> tuplePositionToPixelYX

                        Up ->
                            generateValue pathPoint.point.y pathPoint.point.x (\y1 -> (-) (y1 + Area.fieldSize)) |> tuplePositionToPixelYX
                    )
    in
    distanceToFieldAndRatio path distance
        |> internal
