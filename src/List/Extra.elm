module List.Extra exposing (removeNothing)


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
