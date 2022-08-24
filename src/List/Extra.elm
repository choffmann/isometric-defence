module List.Extra exposing (last, removeNothing, snoc)

import Html exposing (a)


removeNothing : List (Maybe a) -> List a
removeNothing =
    let
        internal acc list =
            case list of
                [] ->
                    acc

                h :: hs ->
                    case h of
                        Nothing ->
                            internal acc hs

                        Just value ->
                            internal (value :: acc) hs
    in
    internal []


last : List a -> Maybe a
last list =
    let
        internal h hs =
            case hs of
                [] ->
                    h

                r :: rs ->
                    internal r rs
    in
    case list of
        [] ->
            Nothing

        h :: hs ->
            Just (internal h hs)


snoc : List a -> a -> List a
snoc list e =
    case list of
        [] ->
            [ e ]

        x :: xs ->
            x :: snoc xs e
