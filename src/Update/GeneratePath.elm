module Update.GeneratePath exposing (update)

import Area
import List.Extra as List
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Path exposing (Path, PathDirection(..), PathPoint)
import Point exposing (Point)
import Random


checkDirection : Path -> List PathDirection
checkDirection path =
    let
        checkDown direction y =
            case direction of
                Up ->
                    False

                Right ->
                    (y + 2) < Area.heightTiles

                Down ->
                    (y + 2) < Area.heightTiles

        checkUp direction y =
            case direction of
                Up ->
                    (y - 2) > 0

                Right ->
                    (y - 2) > 0

                Down ->
                    False

        internal { direction, point } =
            case direction of
                Right ->
                    if checkDown direction point.y && checkUp direction point.y then
                        [ Right, Up, Down ]

                    else if checkDown direction point.y then
                        [ Right, Down ]

                    else if checkUp direction point.y then
                        [ Right, Up ]

                    else
                        [ Right ]

                Down ->
                    if checkDown direction (point.y + 2) && checkUp direction (point.y + 2) then
                        [ Right, Up, Down ]

                    else if checkDown direction (point.y + 2) then
                        [ Right, Down ]

                    else if checkUp direction (point.y + 2) then
                        [ Right, Up ]

                    else
                        [ Right ]

                Up ->
                    if checkDown direction (point.y - 2) && checkUp direction (point.y - 2) then
                        [ Right, Up, Down ]

                    else if checkDown direction (point.y - 2) then
                        [ Right, Down ]

                    else if checkUp direction (point.y - 2) then
                        [ Right, Up ]

                    else
                        [ Right ]
    in
    case List.head path of
        Nothing ->
            [ Right, Up, Down ]

        Just pathPoint ->
            internal pathPoint


update : Msg -> Model -> ( Model, Cmd Messages.Msg )
update msg model =
    let
        newPath pathPoint path direction =
            case pathPoint.direction of
                Right ->
                    PathPoint (Point (pathPoint.point.x + 2) pathPoint.point.y) direction :: PathPoint (Point (pathPoint.point.x + 1) pathPoint.point.y) Right :: pathPoint :: path

                Up ->
                    PathPoint (Point pathPoint.point.x (pathPoint.point.y - 2)) direction :: PathPoint (Point pathPoint.point.x (pathPoint.point.y - 1)) Up :: pathPoint :: path

                Down ->
                    PathPoint (Point pathPoint.point.x (pathPoint.point.y + 2)) direction :: PathPoint (Point pathPoint.point.x (pathPoint.point.y + 1)) Down :: pathPoint :: path
    in
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
                            case path of
                                [] ->
                                    ( model, Cmd.none )

                                h :: hs ->
                                    if h.point.x + 2 > Area.widthTiles then
                                        ( { model | gameState = Paused, path = Just (PathPoint (Point (h.point.x + 2) h.point.y) direction :: newPath h hs direction) }, Cmd.none )

                                    else
                                        ( { model | path = Just (newPath h hs direction) }, Random.generate PathDirectionGenerate (Path.directionGenerator (checkDirection (newPath h hs direction))) )

                PathPointGenerate point ->
                    ( { model | path = Just [ PathPoint point Right ], gameState = GeneratePath }, Random.generate PathDirectionGenerate (Path.directionGenerator (checkDirection [ PathPoint point Right ])) )

                _ ->
                    ( model, Cmd.none )
