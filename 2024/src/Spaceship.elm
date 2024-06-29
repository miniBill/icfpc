module Spaceship exposing (trySolve)


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
