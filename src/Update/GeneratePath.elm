module Update.GeneratePath exposing (update)

import Area
import List.Nonempty as Nonempty
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (Path, PathDirection(..), PathPoint)
import Point exposing (Point)
import Random


createNeighbor : PathPoint -> Path
createNeighbor point =
    case point.direction of
        Up ->
            Nonempty.singleton (PathPoint (Point point.point.x (point.point.y - 1)) point.direction)

        Down ->
            Nonempty.singleton (PathPoint (Point point.point.x (point.point.y + 1)) point.direction)

        Right ->
            Nonempty.singleton (PathPoint (Point (point.point.x + 1) point.point.y) point.direction)


createPoint : Maybe Path -> PathPoint -> PathDirection -> Path
createPoint path prevPoint direction =
    case path of
        Nothing ->
            Debug.todo ""

        Just justPath ->
            case direction of
                Up ->
                    let
                        newPoint : Point
                        newPoint =
                            Point prevPoint.point.x (prevPoint.point.y - 1)
                    in
                    Nonempty.singleton (PathPoint newPoint direction)
                        |> Nonempty.append (createNeighbor (PathPoint newPoint direction))
                        |> Nonempty.reverse
                        |> Nonempty.append justPath

                Down ->
                    let
                        newPoint : Point
                        newPoint =
                            Point prevPoint.point.x (prevPoint.point.y + 1)
                    in
                    Nonempty.singleton (PathPoint newPoint direction)
                        |> Nonempty.append (createNeighbor (PathPoint newPoint direction))
                        |> Nonempty.reverse
                        |> Nonempty.append justPath

                Right ->
                    let
                        newPoint : Point
                        newPoint =
                            Point (prevPoint.point.x + 1) prevPoint.point.y
                    in
                    Nonempty.singleton (PathPoint newPoint direction)
                        |> Nonempty.append (createNeighbor (PathPoint newPoint direction))
                        |> Nonempty.reverse
                        |> Nonempty.append justPath


createFirstRandomPoint : PathPoint -> Path
createFirstRandomPoint point =
    Nonempty.cons point (createNeighbor point)


checkDirection : Maybe Path -> List PathDirection
checkDirection path =
    case path of
        Nothing ->
            [ Right, Up, Down ]

        Just justPath ->
            let
                checkIsOnPathUp : Bool
                checkIsOnPathUp =
                    Nonempty.any (\e -> e.point.y /= (Nonempty.last justPath).point.y - 1) justPath

                checkIsOnPathDown : Bool
                checkIsOnPathDown =
                    Nonempty.any (\e -> e.point.y /= (Nonempty.last justPath).point.y + 1) justPath

                checkOutOfBoundsUp : Bool
                checkOutOfBoundsUp =
                    (Nonempty.last justPath).point.y - 3 >= 0

                checkOutOfBoundsDown : Bool
                checkOutOfBoundsDown =
                    (Nonempty.last justPath).point.y + 3 <= ((Area.area.height // Area.fieldSize) - 1)
            in
            if
                -- Wenn alle abfragen erfolgreich, kann jede Richtung gewählt werden
                checkIsOnPathUp
                    && checkOutOfBoundsUp
                    && checkIsOnPathDown
                    && checkOutOfBoundsDown
            then
                [ Down, Up, Right ]

            else if
                -- Wenn nur nach oben nicht möglich, kann nur rechts und unten gewählt werden
                (not checkIsOnPathUp || not checkOutOfBoundsUp)
                    && checkIsOnPathDown
                    && checkOutOfBoundsDown
            then
                [ Down, Right ]

            else if
                -- Wenn nur nach unten nicht möglich, kann nur nach oben oder rechts gewählt werden
                (not checkIsOnPathDown || not checkOutOfBoundsDown)
                    && checkIsOnPathUp
                    && checkOutOfBoundsUp
            then
                [ Up, Right ]

            else
                -- Wenn nach oben und unten nicht möglich, kann nur nach rechts gewählt werden
                [ Right ]


checkIsLastPoint : Path -> Bool
checkIsLastPoint path =
    -- TODO: area.width maybe without multiply by fieldSize????
    (Nonempty.last path).point.x + 1 > ((Area.area.width // Area.fieldSize) - 1)


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

                        Just path ->
                            if checkIsLastPoint path then
                                ( { model | gameState = Paused }, Cmd.none )

                            else
                                ( { model | path = Just (createPoint model.path (Nonempty.last path) direction) }, Random.generate PathDirectionGenerate (Path.directionGenerator (checkDirection model.path)) )

                PathPointGenerate point ->
                    ( { model | path = Just (createFirstRandomPoint (PathPoint point Right)) }, Random.generate PathDirectionGenerate (Path.directionGenerator (checkDirection model.path)) )

                Tick _ ->
                    ( model, Cmd.none )

                Key _ ->
                    ( model, Cmd.none )

                Click _ ->
                    ( model, Cmd.none )

                Canvas _ ->
                    ( model, Cmd.none )

                EnterCanvas ->
                    ( model, Cmd.none )

                Event _ ->
                    ( model, Cmd.none )
