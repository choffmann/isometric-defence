module Update.GeneratePath exposing (checkDirection, checkIsLastPoint, createFirstRandomPoint, createNeighbor, createPoint, update)

import Area
import List.Nonempty as Nonempty exposing (Nonempty)
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (Path, PathDirection(..), PathPoint)
import Point exposing (Point)


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


checkDirection : Maybe Path -> Nonempty PathDirection
checkDirection path =
    case path of
        Nothing ->
            Nonempty.cons Down (Nonempty.cons Up (Nonempty.singleton Right))

        Just justPath ->
            let
                checkDirectionUp : Point -> Bool
                checkDirectionUp newPoint =
                    Nonempty.any (\e -> e.point.y == newPoint.y - 1) justPath

                checkDirectionDown : Point -> Bool
                checkDirectionDown newPoint =
                    Nonempty.any (\e -> e.point.y == newPoint.y + 1) justPath

                checkDirectionRight : Point -> Bool
                checkDirectionRight newPoint =
                    Nonempty.any (\e -> e.point.y == newPoint.y + 1) justPath

                checkOutOfBoundsUp : Point -> Bool
                checkOutOfBoundsUp newPoint =
                    newPoint.y - 2 <= 0

                checkOutOfBoundsDown : Point -> Bool
                checkOutOfBoundsDown newPoint =
                    newPoint.y + 2 >= ((Area.area.height // Area.fieldSize) - 1)
            in
            -- TODO: Optimieren
            if
                not (checkDirectionUp (Nonempty.last justPath).point)
                    && not (checkOutOfBoundsUp (Nonempty.last justPath).point)
                    && not (checkDirectionDown (Nonempty.last justPath).point)
                    && not (checkOutOfBoundsDown (Nonempty.last justPath).point)
                    && not (checkDirectionRight (Nonempty.last justPath).point)
            then
                Nonempty.cons Down (Nonempty.cons Up (Nonempty.singleton Right))

            else if
                (checkDirectionUp (Nonempty.last justPath).point || checkOutOfBoundsUp (Nonempty.last justPath).point)
                    && not (checkDirectionDown (Nonempty.last justPath).point)
                    && not (checkOutOfBoundsDown (Nonempty.last justPath).point)
                    && not (checkDirectionRight (Nonempty.last justPath).point)
            then
                Nonempty.cons Down (Nonempty.singleton Right)

            else if
                (checkDirectionDown (Nonempty.last justPath).point || checkOutOfBoundsDown (Nonempty.last justPath).point)
                    && not (checkDirectionUp (Nonempty.last justPath).point)
                    && not (checkOutOfBoundsUp (Nonempty.last justPath).point)
                    && not (checkDirectionRight (Nonempty.last justPath).point)
            then
                Nonempty.cons Up (Nonempty.singleton Right)

            else
                Nonempty.singleton Right


checkIsLastPoint : Path -> Bool
checkIsLastPoint path =
    -- TODO: area.width maybe without multiply by fieldSize????
    (Nonempty.last path).point.x + 1 > ((Area.area.width // Area.fieldSize) - 1)


update : Model -> ( Model, Cmd Msg )
update model =
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
            case model.path of
                Nothing ->
                    ( model, Cmd.none )

                Just path ->
                    if Nonempty.length path > 10 then
                        ( { model | gameState = Paused }, Cmd.none )

                    else
                        ( model, Cmd.none )
