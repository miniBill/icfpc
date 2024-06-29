module Backend exposing (app)

import Env
import Http
import Lamdera
import Types exposing (BackendModel, BackendMsg, ToBackend(..), ToFrontend(..))


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( {}, Cmd.none )


update : BackendMsg -> backendModel -> ( backendModel, Cmd BackendMsg )
update ( clientId, toFrontend ) model =
    ( model, Lamdera.sendToFrontend clientId (TF toFrontend) )


updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> backendModel -> ( backendModel, Cmd BackendMsg )
updateFromFrontend _ cid (TB msg) model =
    ( model
    , Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Env.token) ]
        , url = "https://boundvariable.space/communicate"
        , body = Http.stringBody "text/plain" msg
        , expect = Http.expectString (\res -> ( cid, res ))
        , timeout = Nothing
        , tracker = Nothing
        }
    )


subscriptions : backendModel -> Sub msg
subscriptions _ =
    Sub.none
