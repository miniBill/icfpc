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
        |> Result.andThen
            (\inputString ->
                if String.startsWith ("get " ++ prefix) inputString then
                    Ok <| String.dropLeft (4 + String.length prefix) inputString

                else
                    Err <| "Input string does not start with `get " ++ prefix
            )
        |> Result.andThen
            (\levelString ->
                case String.toInt levelString of
                    Just level ->
                        Ok level

                    Nothing ->
                        Err (levelString ++ " is not a valid int")
            )
        |> Just
