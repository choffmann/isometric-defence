module List.Extra exposing (removeNothing, snoc)


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


snoc : List a -> a -> List a
snoc list e =
    case list of
        [] ->
            [ e ]

        x :: xs ->
            x :: snoc xs e
