module Update.GeneratePath exposing (..)

import Area exposing (area, fieldSize)
import List.Nonempty exposing (Nonempty, any, append, cons, fromList, last, length, reverse, singleton)
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (Path, PathDirection(..), PathPoint, directionGenerator, pointGenerator)
import Point exposing (Point)
import Random


createNeighbor : PathPoint -> Path
createNeighbor point =
    case point.direction of
        Up ->
            singleton (PathPoint (Point point.point.x (point.point.y - 1)) point.direction)

        Down ->
            singleton (PathPoint (Point point.point.x (point.point.y + 1)) point.direction)

        Right ->
            singleton (PathPoint (Point (point.point.x + 1) point.point.y) point.direction)


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
                    singleton (PathPoint newPoint direction)
                        |> append (createNeighbor (PathPoint newPoint direction))
                        |> reverse
                        |> append justPath

                Down ->
                    let
                        newPoint : Point
                        newPoint =
                            Point prevPoint.point.x (prevPoint.point.y + 1)
                    in
                    singleton (PathPoint newPoint direction)
                        |> append (createNeighbor (PathPoint newPoint direction))
                        |> reverse
                        |> append justPath

                Right ->
                    let
                        newPoint : Point
                        newPoint =
                            Point (prevPoint.point.x + 1) prevPoint.point.y
                    in
                    singleton (PathPoint newPoint direction)
                        |> append (createNeighbor (PathPoint newPoint direction))
                        |> reverse
                        |> append justPath


createFirstRandomPoint : PathPoint -> Path
createFirstRandomPoint point =
    cons point (createNeighbor point)


checkDirection : Maybe Path -> Nonempty PathDirection
checkDirection path =
    case path of
        Nothing ->
            cons Down (cons Up (singleton Right))

        Just justPath ->
            let
                checkDirectionUp : Point -> Bool
                checkDirectionUp newPoint =
                    any (\e -> e.point.y == newPoint.y - 1) justPath

                checkDirectionDown : Point -> Bool
                checkDirectionDown newPoint =
                    any (\e -> e.point.y == newPoint.y + 1) justPath

                checkDirectionRight : Point -> Bool
                checkDirectionRight newPoint =
                    any (\e -> e.point.y == newPoint.y + 1) justPath

                checkOutOfBoundsUp : Point -> Bool
                checkOutOfBoundsUp newPoint =
                    newPoint.y - 4 <= 0

                checkOutOfBoundsDown : Point -> Bool
                checkOutOfBoundsDown newPoint =
                    newPoint.y + 4 >= ((area.height // fieldSize) - 1)
            in
            -- TODO: Optimieren
            if
                (checkDirectionUp (last justPath).point || checkOutOfBoundsUp (last justPath).point)
                    && (checkDirectionDown (last justPath).point || checkOutOfBoundsDown (last justPath).point)
                    && not (checkDirectionRight (last justPath).point)
            then
                singleton Right

            else if
                (checkDirectionUp (last justPath).point || checkOutOfBoundsUp (last justPath).point)
                    && checkDirectionRight (last justPath).point
                    && not (checkOutOfBoundsDown (last justPath).point)
                    && not (checkDirectionDown (last justPath).point)
            then
                singleton Down

            else if
                (checkDirectionDown (last justPath).point || checkOutOfBoundsDown (last justPath).point)
                    && checkDirectionRight (last justPath).point
                    && not (checkOutOfBoundsUp (last justPath).point)
                    && not (checkDirectionUp (last justPath).point)
            then
                singleton Up

            else if
                checkDirectionRight (last justPath).point
                    && not (checkOutOfBoundsDown (last justPath).point)
                    && not (checkOutOfBoundsUp (last justPath).point)
                    && not (checkDirectionDown (last justPath).point)
                    && not (checkDirectionUp (last justPath).point)
            then
                cons Up (singleton Down)

            else if
                (checkDirectionUp (last justPath).point || checkOutOfBoundsUp (last justPath).point)
                    && not (checkOutOfBoundsDown (last justPath).point)
            then
                cons Right (singleton Down)

            else if
                (checkDirectionDown (last justPath).point || checkOutOfBoundsDown (last justPath).point)
                    && not (checkOutOfBoundsUp (last justPath).point)
            then
                cons Up (singleton Right)

            else
                cons Down (cons Up (singleton Right))


checkIsLastPoint : Maybe Path -> Bool
checkIsLastPoint path =
    case path of
        Nothing ->
            Debug.todo ""

        Just justPath ->
            -- TODO: area.width maybe without multiply by fieldSize????
            (last justPath).point.x <= (area.width // fieldSize)


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
                    if length path > 10 then
                        ( { model | gameState = Paused }, Cmd.none )

                    else
                        ( model, Cmd.none )
