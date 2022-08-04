port module Utils.Ports exposing (changeFullScreen, onFullScreenChange)

import FullScreenMode exposing (FullScreenMode(..))
import Json.Encode as Encode
import Messages exposing (Msg(..), ReceivingEvents(..), SendingEvents(..))


type alias EventMsg =
    { message : String
    , event : String
    }


port sendEventMessage : Encode.Value -> Cmd msg


sendEventMessageEncoder : SendingEvents -> Encode.Value
sendEventMessageEncoder event =
    case event of
        ChangeFullScreen msg ->
            Encode.object
                [ ( "event", Encode.string "changeFullScreen" )
                , ( "message"
                  , Encode.string
                        (case msg of
                            Open ->
                                "open"

                            Close ->
                                "close"
                        )
                  )
                ]


changeFullScreen : SendingEvents -> Cmd msg
changeFullScreen msg =
    sendEventMessage (sendEventMessageEncoder msg)


port eventMessageReceiver : (EventMsg -> msg) -> Sub msg


onFullScreenChange : Sub Msg
onFullScreenChange =
    eventMessageReceiver
        (\msg ->
            case msg.event of
                "fullScreenChanged" ->
                    case msg.message of
                        "opened" ->
                            Event (FullScreenChanged Open)

                        "closed" ->
                            Event (FullScreenChanged Close)

                        _ ->
                            Event UnknownEvent

                _ ->
                    Event UnknownEvent
        )
