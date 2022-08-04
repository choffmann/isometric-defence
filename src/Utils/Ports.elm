port module Utils.Ports exposing (changeFullScreen, onEventMessage)

import FullScreenMode exposing (FullScreenMode(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Messages exposing (Msg(..), ReceivingEvents(..), SendingEvents(..))
import Utils.Decoder exposing (receiveEventDecoder)


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


port eventMessageReceiver : (Decode.Value -> msg) -> Sub msg


onEventMessage : Sub Msg
onEventMessage =
    eventMessageReceiver
        (\event ->
            case Decode.decodeValue receiveEventDecoder event of
                Err _ ->
                    Event UnknownEvent

                Ok parsedEvent ->
                    Event parsedEvent
        )
