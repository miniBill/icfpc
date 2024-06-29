module Spaceship exposing (solve)

import Icfp exposing (Icfp)
import List.Extra
import Result.Extra


solve : String -> Icfp -> Maybe (Result String String)
solve input response =
    Maybe.map2 (Result.map2 Tuple.pair)
        (getLevel input)
        (getCoords response)
        |> Maybe.map
            (Result.andThen
                (\( level, coords ) ->
                    coords
                        |> trySolve 1000 ( 0, 0 ) ( 0, 0 ) []
                        |> Result.map
                            (\moves ->
                                "solve spaceship" ++ String.fromInt level ++ " " ++ String.concat (List.map String.fromInt moves)
                            )
                )
            )
        |> Debug.log "Spaceship.solve"


getLevel : String -> Maybe (Result String Int)
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
        |> Just


getCoords : Icfp -> Maybe (Result String (List ( Int, Int )))
getCoords response =
    case response of
        Icfp.String coords ->
            coords
                |> parseCoords
                |> Just

        _ ->
            Just (Err "response is not a string")


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


trySolve : Int -> ( Int, Int ) -> ( Int, Int ) -> List Int -> List ( Int, Int ) -> Result String (List Int)
trySolve budget ( x, y ) ( vx, vy ) moves coords =
    if budget < 0 then
        Err <| "Out of budget: " ++ Debug.toString (List.reverse moves)

    else
        case coords of
            [] ->
                Ok (List.reverse moves)

            ( tx, ty ) :: tail ->
                let
                    go : Int -> Int -> Int -> ( Int, Int, Int )
                    go position speed target =
                        let
                            braked : Int
                            braked =
                                position + (speed * speed + speed) // 2

                            wrongDirection : Bool
                            wrongDirection =
                                speed * (target - position) < 0

                            accel : Int
                            accel =
                                if
                                    wrongDirection
                                        || (speed > 0 && braked >= target)
                                        || (speed < 0 && braked <= target)
                                then
                                    -(sign speed)

                                else
                                    sign (target - position)

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
                trySolve (budget - 1)
                    ( nx, ny )
                    ( nvx, nvy )
                    (move :: moves)
                    (if nx == tx && ny == ty then
                        tail

                     else
                        coords
                    )


sign : number -> number2
sign x =
    if x < 0 then
        -1

    else if x > 0 then
        1

    else
        0
