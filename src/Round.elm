module Round exposing (Round(..), roundToInt)


type Round
    = Round1
    | Round2
    | Round3
    | Round4
    | Round5
    | Round6
    | Round7


roundToInt : Round -> Int
roundToInt round =
    case round of
        Round1 ->
            1

        Round2 ->
            2

        Round3 ->
            3

        Round4 ->
            4

        Round5 ->
            5

        Round6 ->
            6

        Round7 ->
            7
