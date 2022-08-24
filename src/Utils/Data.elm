module Utils.Data exposing (..)


type Load a
    = Loading
    | Success a
    | Failure
