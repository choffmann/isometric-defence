module Main exposing (Model, main)

import Area exposing (Area)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown)
import Canvas exposing (Point, Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Enemy exposing (Enemies(..), Enemy, toEnemy)
import Html exposing (Html, div, text)
import Json.Decode as Decode exposing (Decoder)
import Point exposing (Point)
import Styling
import Tower exposing (Tower, Towers(..), toTower)


type Msg
    = Tick Float
    | Key Key
    | Click Point


type Key
    = Space
    | UnknownKey


apply : Decoder a -> Decoder (a -> b) -> Decoder b
apply =
    Decode.map2 (|>)


keyDecoder : Decoder Msg
keyDecoder =
    let
        toKey k =
            case k of
                " " ->
                    Space

                _ ->
                    UnknownKey
    in
    Decode.field "key" Decode.string
        |> Decode.map toKey
        |> Decode.map Key


clickDecoder : Decoder Msg
clickDecoder =
    Decode.succeed Point
        |> apply (Decode.field "pageX" Decode.int)
        |> apply (Decode.field "pageY" Decode.int)
        |> Decode.map Click


type GameState
    = Running
    | Paused
    | Won
    | Lost


type alias Model =
    { gameState : GameState
    , hp : Int
    , money : Int
    , enemies : List Enemy
    , towers : List Tower
    , delta : Float
    }


type alias Flags =
    { msg : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gameState = Paused
      , hp = 1000
      , money = 0
      , enemies = [ toEnemy Soldat, toEnemy Soldat, toEnemy Soldat, toEnemy Soldat, toEnemy Soldat ]
      , towers = [ toTower Basic, toTower Basic ]
      , delta = 0
      }
    , Cmd.none
    )


canvas : Model -> Area -> List Renderable
canvas model area =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat area.width) (toFloat area.width) ]
    , shapes [ fill (Color.rgba 255 0 0 1) ] [ rect ( 0, 0 ) 100 50 ]
    ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (Debug.toString model) ]
        , div Styling.canvasContainerStyles
            [ div
                Styling.canvasStyles
                [ Canvas.toHtml
                    ( Area.area.width, Area.area.height )
                    []
                    (canvas model Area.area)
                ]
            ]
        ]


damageEnemies : Tower -> List Enemy -> ( Tower, List Enemy )
damageEnemies =
    let
        internal : List Enemy -> Tower -> List Enemy -> ( Tower, List Enemy )
        internal acc tower enemyList =
            case enemyList of
                [] ->
                    ( tower, List.sortBy (\enemy -> enemy.distance) acc )

                enemy :: hs ->
                    if tower.lastShot > tower.attackSpeed && inRange tower.position tower.attackRadius enemy.position then
                        internal (({ enemy | hp = enemy.hp - tower.damage } :: acc) ++ hs) { tower | lastShot = 0 } []

                    else
                        internal (enemy :: acc) tower hs
    in
    internal []


damage : List Tower -> List Enemy -> ( List Tower, List Enemy )
damage =
    let
        internal : List Tower -> List Tower -> List Enemy -> ( List Tower, List Enemy )
        internal acc towers enemies =
            case towers of
                [] ->
                    ( acc, enemies )

                tower :: hs ->
                    if tower.lastShot > tower.attackSpeed then
                        case damageEnemies tower enemies of
                            ( newTower, newEnemies ) ->
                                internal (newTower :: acc) hs newEnemies

                    else
                        internal (tower :: acc) hs enemies
    in
    internal []


killEnemies : List Enemy -> List Enemy
killEnemies =
    List.filter (\enemy -> enemy.hp > 0)


inRange : Point -> Int -> Point -> Bool
inRange poin1 radius point2 =
    True


type alias Path =
    List Point


getPosition : Path -> Int -> Point
getPosition path distance =
    Point 0 0


moveEnemies : Path -> List Enemy -> List Enemy
moveEnemies path =
    List.map
        (\enemy ->
            { enemy
                | distance = enemy.distance + enemy.speed
                , position = getPosition path (enemy.distance + enemy.speed)
            }
        )


cooldownTowers : Float -> List Tower -> List Tower
cooldownTowers delta =
    List.map (\tower -> { tower | lastShot = tower.lastShot + delta })


tick : Model -> Float -> Model
tick model delta =
    let
        moneyfromKilledEnemies enemies =
            List.foldl
                (\enemy money ->
                    if enemy.hp <= 0 then
                        money + enemy.worth

                    else
                        money
                )
                0
                enemies
    in
    case damage model.towers model.enemies of
        ( towers, enemies ) ->
            case enemies |> moveEnemies [ Point 0 0 ] of
                newEnemies ->
                    { model
                        | towers = cooldownTowers delta towers
                        , enemies = killEnemies newEnemies
                        , money = model.money + moneyfromKilledEnemies newEnemies
                        , delta = delta
                    }


updateRunning : Msg -> Model -> Model
updateRunning msg model =
    case msg of
        Tick delta ->
            tick model delta

        Key key ->
            case key of
                Space ->
                    { model | gameState = Paused }

                UnknownKey ->
                    model

        Click point ->
            model


updatePaused : Msg -> Model -> Model
updatePaused msg model =
    case msg of
        Tick delta ->
            { model | delta = delta }

        Key key ->
            case key of
                Space ->
                    { model | gameState = Running }

                UnknownKey ->
                    model

        Click point ->
            model


updateWon : Msg -> Model -> Model
updateWon msg model =
    case msg of
        Tick delta ->
            { model | delta = delta }

        Key key ->
            model

        Click point ->
            model


updateLost : Msg -> Model -> Model
updateLost msg model =
    case msg of
        Tick delta ->
            { model | delta = delta }

        Key key ->
            model

        Click point ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case model.gameState of
        Running ->
            updateRunning msg model

        Paused ->
            updatePaused msg model

        Won ->
            updateWon msg model

        Lost ->
            updateLost msg model
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown keyDecoder
        , onClick clickDecoder
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
