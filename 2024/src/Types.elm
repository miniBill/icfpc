module Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg(..), Response(..), ToBackend(..), ToFrontend(..))

import Http
import Lamdera exposing (ClientId)


type alias FrontendModel =
    { input : String
    , response : Response
    , solution : Maybe (Result String String)
    }


type Response
    = NotAsked
    | Asking
    | Error Http.Error
    | Response String


type FrontendMsg
    = Input String
    | Send String
    | OnUrlChange Lamdera.Url
    | OnUrlRequest Lamdera.UrlRequest
    | StepReduceResponse Int
    | Solve
    | ChangedResponse String


type ToBackend
    = TB String


type alias BackendModel =
    {}


type alias BackendMsg =
    ( ClientId, Result Http.Error String )


type ToFrontend
    = TF (Result Http.Error String)
