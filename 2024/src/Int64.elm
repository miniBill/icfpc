module Int64 exposing (Int64, add, div, fromInt, fromString, lessThan, moreThan, mul, negate, remainderBy, sub, toInt53, toString, zero)

import UInt64 exposing (UInt64)



{- True is positive, False is negative -}


type alias Int64 =
    ( Bool, UInt64 )


zero : Int64
zero =
    ( True, UInt64.zero )


fromInt : Int -> ( Bool, UInt64 )
fromInt i =
    if i < 0 then
        ( False, UInt64.fromInt -i )

    else
        ( True, UInt64.fromInt i )


add : Int64 -> Int64 -> Int64
add ( sl, l ) ( sr, r ) =
    if sl == sr then
        ( sl, UInt64.add l r )

    else
        case UInt64.compare l r of
            GT ->
                ( sl, UInt64.sub l r )

            LT ->
                ( not sl, UInt64.sub r l )

            EQ ->
                zero


sub : Int64 -> Int64 -> Int64
sub l r =
    add l (negate r)


mul : Int64 -> Int64 -> Int64
mul ( sl, l ) ( sr, r ) =
    ( sl == sr, UInt64.mul l r )


div : Int64 -> Int64 -> Int64
div ( sl, l ) ( sr, r ) =
    ( sl == sr, UInt64.div l r )


remainderBy : Int64 -> Int64 -> Int64
remainderBy r l =
    div l r
        |> mul r
        |> sub l


lessThan : Int64 -> Int64 -> Bool
lessThan l r =
    compare l r == LT


moreThan : Int64 -> Int64 -> Bool
moreThan l r =
    compare l r == GT


compare : Int64 -> Int64 -> Order
compare ( ls, l ) ( rs, r ) =
    if ls then
        if rs then
            -- Both positive
            UInt64.compare l r

        else
            GT

    else if rs then
        LT

    else
        -- Both negative, intentionally inverted
        UInt64.compare r l


negate : Int64 -> Int64
negate (( s, v ) as i) =
    if isZero i then
        i

    else
        ( not s, v )


toInt53 : Int64 -> Maybe Int
toInt53 ( s, v ) =
    Maybe.map (\w -> toSign s * w) (UInt64.toInt53 v)


toSign : Bool -> Int
toSign b =
    if b then
        1

    else
        -1


toString : Int64 -> String
toString ( s, i ) =
    if s then
        UInt64.toString i

    else
        "-" ++ UInt64.toString i


fromString : String -> Maybe Int64
fromString input =
    if String.startsWith "-" input then
        Maybe.map (Tuple.pair False) (UInt64.fromString (String.dropLeft 1 input))

    else
        Maybe.map (Tuple.pair True) (UInt64.fromString input)


isZero : Int64 -> Bool
isZero ( s, v ) =
    UInt64.isZero v
