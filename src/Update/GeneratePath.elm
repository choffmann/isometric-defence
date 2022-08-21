module Update.GeneratePath exposing (update)

import Area
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (Path(..), PathDirection(..), PathPoint)
import Point exposing (Point)
import Random


createNeighbor : PathPoint -> PathPoint
createNeighbor point =
    case point.direction of
        Up ->
            PathPoint (Point point.point.x (point.point.y - 1)) point.direction

        Down ->
            PathPoint (Point point.point.x (point.point.y + 1)) point.direction

        Right ->
            PathPoint (Point (point.point.x + 1) point.point.y) point.direction


createPoint : Path -> PathDirection -> Path
createPoint (Last prevPoint path) direction =
    case direction of
        Up ->
            let
                newPoint : Point
                newPoint =
                    Point prevPoint.point.x (prevPoint.point.y - 1)
            in
            Path.addPathPoint (PathPoint newPoint direction) (Last prevPoint path)
                |> Path.addPathPoint (createNeighbor (PathPoint newPoint direction))

        Down ->
            let
                newPoint : Point
                newPoint =
                    Point prevPoint.point.x (prevPoint.point.y + 1)
            in
            Path.addPathPoint (PathPoint newPoint direction) (Last prevPoint path)
                |> Path.addPathPoint (createNeighbor (PathPoint newPoint direction))

        Right ->
            let
                newPoint : Point
                newPoint =
                    Point (prevPoint.point.x + 1) prevPoint.point.y
            in
            Path.addPathPoint (PathPoint newPoint direction) (Last prevPoint path)
                |> Path.addPathPoint (createNeighbor (PathPoint newPoint direction))


createFirstRandomPoint : PathPoint -> Path
createFirstRandomPoint point =
    createPoint (Last point []) point.direction


checkDirection : Maybe Path -> List PathDirection
checkDirection path =
    case path of
        Nothing ->
            [ Right, Down, Up ]

        Just (Last prevPoint justPath) ->
            let
                checkIsOnPathUp : Bool
                checkIsOnPathUp =
                    not (List.any (\{ point } -> point.y == prevPoint.point.y - 1) justPath)

                checkIsOnPathDown : Bool
                checkIsOnPathDown =
                    not (List.any (\{ point } -> point.y == prevPoint.point.y + 1) justPath)

                checkOutOfBoundsUp : Bool
                checkOutOfBoundsUp =
                    prevPoint.point.y - 2 >= 0

                checkOutOfBoundsDown : Bool
                checkOutOfBoundsDown =
                    prevPoint.point.y + 2 <= ((Area.area.height // Area.fieldSize) - 1)
            in
            if
                -- Wenn alle abfragen erfolgreich, kann jede Richtung gewählt werden
                checkIsOnPathUp
                    && checkOutOfBoundsUp
                    && checkIsOnPathDown
                    && checkOutOfBoundsDown
            then
                let
                    _ =
                        Debug.log "checkDirection" ("[ Down, Up, Right ]" ++ " with Path: " ++ Debug.toString path)
                in
                [ Down, Up, Right ]

            else if
                -- Wenn nur nach oben nicht möglich, kann nur unten und rechts gewählt werden
                (not checkIsOnPathUp || not checkOutOfBoundsUp)
                    && checkIsOnPathDown
                    && checkOutOfBoundsDown
            then
                let
                    _ =
                        Debug.log "checkDirection" ("[ Down, Right ]" ++ " with Path: " ++ Debug.toString path)
                in
                [ Down, Right ]

            else if
                -- Wenn nur nach unten nicht möglich, kann nur nach oben oder rechts gewählt werden
                (not checkIsOnPathDown || not checkOutOfBoundsDown)
                    && checkIsOnPathUp
                    && checkOutOfBoundsUp
            then
                let
                    _ =
                        Debug.log "checkDirection" ("[ Up, Right ]" ++ " with Path: " ++ Debug.toString path)
                in
                [ Up, Right ]

            else
                -- Wenn nach oben und unten nicht möglich, kann nur nach rechts gewählt werden
                let
                    _ =
                        Debug.log "checkDirection" ("[ Right ]" ++ " with Path: " ++ Debug.toString path)
                in
                [ Right ]


checkIsLastPoint : Path -> Bool
checkIsLastPoint (Last prevPoint _) =
    -- TODO: area.width maybe without multiply by fieldSize????
    prevPoint.point.x + 1 > ((Area.area.width // Area.fieldSize) - 1)


update : Msg -> Model -> ( Model, Cmd Messages.Msg )
update msg model =
    case model.gameState of
        Running ->
            ( model, Cmd.none )

        Paused ->
            ( model, Cmd.none )

        Won ->
            ( model, Cmd.none )

        Lost ->
            ( model, Cmd.none )

        Model.GeneratePath ->
            case msg of
                PathDirectionGenerate direction ->
                    case model.path of
                        Nothing ->
                            ( model, Cmd.none )

                        Just (Last prevPoint path) ->
                            if checkIsLastPoint (Last prevPoint path) then
                                ( { model | gameState = Paused }, Cmd.none )

                            else
                                ( { model | path = Just (createPoint (Last prevPoint path) direction) }, Random.generate PathDirectionGenerate (Path.directionGenerator (checkDirection (Just (Last prevPoint path)))) )

                PathPointGenerate point ->
                    ( { model | path = Just (createFirstRandomPoint (PathPoint point Right)) }, Random.generate PathDirectionGenerate (Path.directionGenerator (checkDirection model.path)) )

                _ ->
                    ( model, Cmd.none )
