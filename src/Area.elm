module Area exposing (Area, area, fieldSize)


fieldSize : Int
fieldSize =
    30


type alias Area =
    { width : Int
    , height : Int
    }


area : Area
area =
    Area (25 * fieldSize) (25 * fieldSize)
