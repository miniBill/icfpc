module Frontend exposing (app)

import Browser
import Element exposing (Element, alignRight, column, el, fill, paragraph, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icfp
import Icfp.Step as Step
import Icfp.ToElm
import Lamdera
import Maybe.Extra
import Problems.Lambdaman
import Problems.Spaceship
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
      , solution = Nothing
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
                    , onPress = Just (Send model.input)
                    }

            Err _ ->
                Element.none
        , text "Response"
        ]
            ++ viewResponse model


viewError : String -> Element msg
viewError err =
    err
        |> String.split "\n"
        |> List.map (\line -> paragraph [] [ text line ])
        |> column
            [ Background.color <| rgb 1 0.8 0.8
            , Border.width 1
            , Theme.padding
            , Font.family [ Font.monospace ]
            ]


viewResponse : FrontendModel -> List (Element FrontendMsg)
viewResponse { response, solution } =
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
                common : List (Element FrontendMsg)
                common =
                    [ Input.multiline []
                        { text = res
                        , placeholder = Nothing
                        , onChange = ChangedResponse
                        , spellcheck = False
                        , label = Input.labelHidden "Response"
                        }
                    ]

                specific : List (Element FrontendMsg)
                specific =
                    case Icfp.parse res of
                        Ok icfp ->
                            row [ Theme.spacing, width fill ]
                                [ Theme.button []
                                    { onPress = Just (StepReduceResponse 1)
                                    , label = text "Reduce (step)"
                                    }
                                , Theme.button []
                                    { onPress = Just (StepReduceResponse 100)
                                    , label = text "Reduce (100)"
                                    }
                                , Theme.button []
                                    { onPress = Just (StepReduceResponse Step.defaultBudget)
                                    , label = text <| "Reduce (" ++ String.fromInt Step.defaultBudget ++ ")"
                                    }
                                , Theme.button []
                                    { onPress = Just Solve
                                    , label = text "Solve"
                                    }
                                , Theme.button []
                                    { onPress = Just ToElm
                                    , label = text "To Elm"
                                    }
                                ]
                                :: (case solution of
                                        Nothing ->
                                            []

                                        Just (Ok sol) ->
                                            [ row [ width fill, Theme.spacing ]
                                                [ text "Solution"
                                                , Theme.button [ alignRight ]
                                                    { onPress = Just (Send (Icfp.toString <| Icfp.String sol))
                                                    , label = text "Send"
                                                    }
                                                ]
                                            , paragraph [] [ text sol ]
                                            ]

                                        Just (Err err) ->
                                            [ text "Solution"
                                            , viewError err
                                            ]
                                   )
                                ++ [ Icfp.view 0 icfp ]

                        Err e ->
                            [ viewError (Debug.toString e) ]
            in
            common ++ specific


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        Input input ->
            ( { model | input = input }, Cmd.none )

        ChangedResponse response ->
            ( { model | response = Response response }, Cmd.none )

        Send message ->
            ( { model
                | response = Asking
                , solution = Nothing
              }
            , Lamdera.sendToBackend (TB message)
            )

        StepReduceResponse budget ->
            case model.response of
                Response raw ->
                    case Icfp.parse raw of
                        Ok icfp ->
                            ( { model | response = Response (Icfp.toString <| Step.reduceWithBudget budget icfp) }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnUrlChange _ ->
            ( model, Cmd.none )

        OnUrlRequest _ ->
            ( model, Cmd.none )

        Solve ->
            case model.response of
                Response response ->
                    case Icfp.parse response of
                        Ok icfp ->
                            ( { model
                                | solution =
                                    Problems.Spaceship.solve model.input icfp
                                        |> Maybe.Extra.orElseLazy (\_ -> Problems.Lambdaman.solve model.input icfp)
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | solution = Just (Err "Can't solve: response is invalid") }
                            , Cmd.none
                            )

                _ ->
                    ( { model | solution = Just (Err "Can't solve: no response") }
                    , Cmd.none
                    )

        ToElm ->
            case model.response of
                Response response ->
                    case Icfp.parse response of
                        Ok icfp ->
                            ( { model | solution = Just (Ok (Icfp.ToElm.toElm icfp)) }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | solution = Just (Err "Can't convert to Elm: response is invalid") }
                            , Cmd.none
                            )

                _ ->
                    ( { model | solution = Just (Err "Can't convert to Elm: no response") }
                    , Cmd.none
                    )


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
