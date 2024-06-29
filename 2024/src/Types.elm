module Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg(..), Response(..), ToBackend(..), ToFrontend(..))

import Http
import Lamdera exposing (ClientId)


type alias FrontendModel =
    { input : String
    , response : Response
    , spaceship : Maybe ( Int, String )
    }


type Response
    = NotAsked
    | Asking
    | Error Http.Error
    | Response String


type FrontendMsg
    = Input String
    | Send
    | OnUrlChange Lamdera.Url
    | OnUrlRequest Lamdera.UrlRequest
    | StepReduceResponse
    | FullyReduceResponse
    | SolveSpaceship Int String


type ToBackend
    = TB String


type alias BackendModel =
    {}


type alias BackendMsg =
    ( ClientId, Result Http.Error String )


type ToFrontend
    = TF (Result Http.Error String)
