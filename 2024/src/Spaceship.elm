module Spaceship exposing (solve, trySolve)

import Icfp exposing (Icfp)
import List.Extra
import Result.Extra


solve : String -> Icfp -> Result String String
solve input response =
    Result.map2 Tuple.pair
        (getLevel input)
        (getCoords response)
        |> andThenOnSecond parseCoords
        |> andThenOnSecond trySolve
        |> Result.map
            (\( level, moves ) ->
                "solve spaceship" ++ String.fromInt level ++ " " ++ String.concat (List.map String.fromInt moves)
            )
        |> Debug.log "spaceshipButton"


getLevel : String -> Result String Int
getLevel input =
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
                if String.startsWith "get spaceship" inputString then
                    Ok (String.dropLeft (String.length "get spaceship") inputString)

                else
                    Err "Input string does not start with `get spaceship`"
            )
        |> Result.andThen
            (\levelString ->
                case String.toInt levelString of
                    Just level ->
                        Ok level

                    Nothing ->
                        Err (levelString ++ " is not a valid int")
            )


getCoords : Icfp -> Result String String
getCoords response =
    case response of
        Icfp.String coords ->
            Ok coords

        _ ->
            Err "response is not a string"


andThenOnSecond : (s -> Result e t) -> Result e ( f, s ) -> Result e ( f, t )
andThenOnSecond t v =
    case v of
        Err e ->
            Err e

        Ok ( f, s ) ->
            case t s of
                Ok ss ->
                    Ok ( f, ss )

                Err e ->
                    Err e


parseCoords : String -> Result String (List ( Int, Int ))
parseCoords input =
    let
        parsePair : String -> Result String ( Int, Int )
        parsePair line =
            case String.split " " line of
                [ l, r ] ->
                    Result.map2
                        Tuple.pair
                        (Result.fromMaybe ("Bad int: " ++ l) (String.toInt l))
                        (Result.fromMaybe ("Bad int: " ++ r) (String.toInt r))

                _ ->
                    Err <| "Wrong number of numbers on line " ++ line
    in
    input
        |> String.split "\n"
        |> List.Extra.removeWhen String.isEmpty
        |> Result.Extra.combineMap parsePair


trySolve : List ( Int, Int ) -> Result String (List Int)
trySolve coords =
    coords
        |> foldlCombine
            (\( tx, ty ) ( ( x, y ), ( vx, vy ), moves ) ->
                let
                    go : Int -> Int -> Int -> ( Int, Int, Int )
                    go position speed target =
                        let
                            unaccellerated : Int
                            unaccellerated =
                                position + speed

                            accel : Int
                            accel =
                                case compare target unaccellerated of
                                    LT ->
                                        -1

                                    EQ ->
                                        0

                                    GT ->
                                        1

                            newSpeed : Int
                            newSpeed =
                                speed + accel

                            newPosition : Int
                            newPosition =
                                position + newSpeed
                        in
                        ( newPosition, newSpeed, accel )

                    ( nx, nvx, ax ) =
                        go x vx tx

                    ( ny, nvy, ay ) =
                        go y vy ty

                    move : Int
                    move =
                        ay * 3 + ax + 5
                in
                Ok ( ( nx, ny ), ( nvx, nvy ), move :: moves )
            )
            ( ( 0, 0 ), ( 0, 0 ), [] )
        |> Result.map (\( _, _, moves ) -> moves)


foldlCombine : (v -> acc -> Result e acc) -> acc -> List v -> Result e acc
foldlCombine f acc list =
    case list of
        [] ->
            Ok acc

        head :: tail ->
            case f head acc of
                Ok v ->
                    foldlCombine f v tail

                next ->
                    next
