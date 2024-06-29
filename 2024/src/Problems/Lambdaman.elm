module Problems.Lambdaman exposing (solve)

import Array exposing (Array)
import Icfp exposing (Icfp)
import Problems.Common as Common


solve : String -> Icfp -> Maybe (Result String String)
solve input response =
    Maybe.map2 (Result.map2 Tuple.pair)
        (Common.getLevel "lambdaman" input)
        (getGrid response)
        |> Maybe.map
            (Result.andThen
                (\( level, grid ) ->
                    grid
                        |> trySolve 10000000
                        |> Result.map
                            (\moves ->
                                "solve lambdaman" ++ String.fromInt level ++ " " ++ moves
                            )
                )
            )
        |> Debug.log "Lambdaman.solve"


type alias Grid =
    Array (Array Char)


getGrid : Icfp -> Maybe (Result String Grid)
getGrid response =
    case response of
        Icfp.String coords ->
            coords
                |> parseGrid
                |> Ok
                |> Just

        _ ->
            Just (Err "response is not a string")


parseGrid : String -> Grid
parseGrid input =
    input
        |> String.split "\n"
        |> List.filterMap
            (\line ->
                if String.isEmpty line then
                    Nothing

                else
                    Just (Array.fromList (String.toList line))
            )
        |> Array.fromList


trySolve : Int -> Grid -> Result String String
trySolve budget grid =
    if budget < 0 then
        Err <| "Out of budget:\n" ++ viewGrid grid

    else
        trySolve (budget - 1) grid


viewGrid : Grid -> String
viewGrid grid =
    grid
        |> Array.toList
        |> List.map
            (\line ->
                line
                    |> Array.toList
                    |> String.fromList
            )
        |> String.join "\n"
