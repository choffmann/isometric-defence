module Shoot exposing (..)

import Area exposing (Field)


type alias Shoot =
    { position : Field
    , hitEnemy : Bool
    , speed : Float
    }
