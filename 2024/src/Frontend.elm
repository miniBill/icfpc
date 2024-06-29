module Frontend exposing (app)

import Browser
import Element exposing (Element, el, fill, paragraph, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Icfp
import Icfp.Step
import Lamdera
import Theme
import Types exposing (FrontendModel, FrontendMsg(..), Response(..), ToBackend(..), ToFrontend(..))


app :
    { init : Lamdera.Url -> Lamdera.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : Lamdera.UrlRequest -> FrontendMsg
    , onUrlChange : Lamdera.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "ICFP - 204"
                , body = [ Element.layout [ width fill ] (view model) ]
                }
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


init : url -> key -> ( FrontendModel, Cmd msg )
init _ _ =
    ( { input = "S'%4}).$%8"
      , response = NotAsked
      }
    , Cmd.none
    )


view : FrontendModel -> Element FrontendMsg
view model =
    Theme.column [ Border.width 0 ] <|
        [ Input.text []
            { text = model.input
            , onChange = Input
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Input"
            }
        , text "Parsed"
        , case Icfp.parse model.input of
            Ok icfp ->
                Element.map
                    (\newIcfp -> Input (Icfp.toString newIcfp))
                    (Icfp.edit icfp)

            Err e ->
                viewError (Debug.toString e)
        , case Icfp.parse model.input of
            Ok _ ->
                Theme.button []
                    { label = text "Send"
                    , onPress = Just Send
                    }

            Err _ ->
                Element.none
        , text "Response"
        ]
            ++ viewResponse model.response


viewError : String -> Element msg
viewError err =
    paragraph
        [ Background.color <| rgb 1 0.8 0.8
        , Border.width 1
        , Theme.padding
        ]
        [ text err ]


viewResponse : Response -> List (Element FrontendMsg)
viewResponse response =
    case response of
        Error e ->
            [ viewError (Debug.toString e)
            ]

        NotAsked ->
            [ text "Not sent yet"
            ]

        Asking ->
            [ el [ Border.width 1, Theme.padding ] <| text "Sending..."
            ]

        Response res ->
            let
                common : List (Element msg)
                common =
                    [ paragraph [ Border.width 1, Theme.padding ] [ text res ]
                    ]

                specific : List (Element FrontendMsg)
                specific =
                    case Icfp.parse res of
                        Ok icfp ->
                            [ row [ Theme.spacing, width fill ]
                                [ Theme.button []
                                    { onPress = Just StepReduceResponse
                                    , label = text "Reduce (step)"
                                    }
                                , Theme.button []
                                    { onPress = Just FullyReduceResponse
                                    , label = text "Reduce (full)"
                                    }
                                ]
                            , Icfp.view 0 icfp
                            ]

                        Err e ->
                            [ viewError (Debug.toString e) ]
            in
            common ++ specific


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        Input input ->
            ( { model | input = input }, Cmd.none )

        Send ->
            ( { model | response = Asking }, Lamdera.sendToBackend (TB model.input) )

        StepReduceResponse ->
            case model.response of
                Response raw ->
                    case Icfp.parse raw of
                        Ok icfp ->
                            ( { model | response = Response (Icfp.toString <| Icfp.Step.step icfp) }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FullyReduceResponse ->
            case model.response of
                Response raw ->
                    case Icfp.parse raw of
                        Ok icfp ->
                            ( { model | response = Response (Icfp.toString <| Icfp.Step.reduce icfp) }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnUrlChange _ ->
            ( model, Cmd.none )

        OnUrlRequest _ ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd msg )
updateFromBackend (TF msg) model =
    case msg of
        Ok response ->
            ( { model | response = Response response }, Cmd.none )

        Err e ->
            ( { model | response = Error e }, Cmd.none )


subscriptions : frontendModel -> Sub msg
subscriptions _ =
    Sub.none
