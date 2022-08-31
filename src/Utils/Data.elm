module Utils.Data exposing (Load(..))


type Load a
    = Loading
    | Success a
    | Failure
