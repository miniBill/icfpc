module Problems.Common exposing (getLevel)

import Icfp


getLevel : String -> String -> Maybe (Result String Int)
getLevel prefix input =
    Icfp.parse input
        |> Result.mapError Debug.toString
        |> Result.andThen
            (\parsedInput ->
                case parsedInput of
                    Icfp.String inputString ->
                        Ok inputString

                    _ ->
                        Err "Input is not a string"
            )
        |> Result.map
            (\inputString ->
                if String.startsWith ("get " ++ prefix) inputString then
                    Just <| String.dropLeft (4 + String.length prefix) inputString

                else
                    Nothing
            )
        |> transpose
        |> Maybe.map
            (Result.andThen
                (\levelString ->
                    case String.toInt levelString of
                        Just level ->
                            Ok level

                        Nothing ->
                            Err (levelString ++ " is not a valid int")
                )
            )


transpose : Result e (Maybe v) -> Maybe (Result e v)
transpose arg =
    case arg of
        Err e ->
            Just (Err e)

        Ok Nothing ->
            Nothing

        Ok (Just v) ->
            Just (Ok v)
