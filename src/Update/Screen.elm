module Update.Screen exposing (..)

import Model exposing (Model)
import Screen exposing (Screen(..))


update : Screen -> Model -> ( Model, Cmd msg )
update screen model =
    ( { model | screen = screen }, Cmd.none )



{- case screen of
   StartScreen ->
       ( { model | screen = screen }, Cmd.none )

   PlayScreen ->
       ( model, Cmd.none )

   PauseScreen ->
       ( model, Cmd.none )

   WonScreen ->
       ( model, Cmd.none )

   LostScreen ->
       ( model, Cmd.none )
-}
