module Icfp.Step exposing (defaultBudget, reduceWithBudget, step)

import Icfp exposing (Binary(..), Icfp(..), Unary(..), decodeInt, decodeString, encodeInt, encodeString)
import Int64 exposing (Int64)
import String exposing (replace)
import UInt64 exposing (UInt64)


step : Icfp -> Result UInt64 Icfp
step icfp =
    case icfp of
        Unary op c ->
            let
                unary resCtor toRes cparser =
                    case cparser c of
                        Just cvalue ->
                            Ok <| resCtor <| toRes cvalue

                        Nothing ->
                            Result.map (Unary op) (step c)
            in
            case op of
                Not ->
                    unary Bool not bool

                Negation ->
                    unary Int Int64.negate int

                IntToString ->
                    unary String
                        (\( s, i ) ->
                            if s then
                                i
                                    |> encodeInt
                                    |> decodeString

                            else
                                Debug.todo "IntToString on a negative number"
                        )
                        int

                StringToInt ->
                    unary Int
                        (\s ->
                            ( True
                            , s
                                |> encodeString
                                |> decodeInt
                            )
                        )
                        string

        Binary op l r ->
            let
                stepLeft () =
                    Result.map (\newL -> Binary op newL r) (step l)

                stepRight () =
                    Result.map (\newR -> Binary op l newR) (step r)

                binary resCtor toRes lparser rparser =
                    case lparser l of
                        Just lvalue ->
                            case rparser r of
                                Just rvalue ->
                                    Ok <| resCtor <| toRes lvalue rvalue

                                Nothing ->
                                    case r of
                                        Variable v ->
                                            Err v

                                        _ ->
                                            stepRight ()

                        Nothing ->
                            case l of
                                Variable v ->
                                    Err v

                                _ ->
                                    stepLeft ()
            in
            case op of
                Equals ->
                    case int l of
                        Just li ->
                            case int r of
                                Just ri ->
                                    Ok (Bool (li == ri))

                                _ ->
                                    stepRight ()

                        Nothing ->
                            case bool l of
                                Just lb ->
                                    case bool r of
                                        Just rb ->
                                            Ok (Bool (lb == rb))

                                        Nothing ->
                                            stepRight ()

                                Nothing ->
                                    binary Bool (==) string string

                CallByName ->
                    binary identity replace lambda Just

                CallStrict ->
                    binary identity replace lambda reduced

                CallLazy ->
                    case lambda l of
                        Nothing ->
                            stepLeft ()

                        Just ( v, b ) ->
                            case step b of
                                Err missing ->
                                    if v == missing then
                                        step r
                                            |> Result.map
                                                (\newR ->
                                                    if newR == r then
                                                        replace ( v, b ) r

                                                    else
                                                        Binary CallLazy l newR
                                                )

                                    else
                                        Err missing

                                Ok newB ->
                                    if newB == b then
                                        Ok (replace ( v, b ) r)

                                    else
                                        Ok (Binary CallLazy (Lambda v newB) r)

                Drop ->
                    binary String
                        (\i ->
                            case Int64.toInt53 i of
                                Just ii ->
                                    String.dropLeft ii

                                Nothing ->
                                    Debug.todo "Number too big for Drop"
                        )
                        int
                        string

                Take ->
                    binary String
                        (\i ->
                            case Int64.toInt53 i of
                                Just ii ->
                                    String.left ii

                                Nothing ->
                                    Debug.todo "Number too big for Take"
                        )
                        int
                        string

                Concat ->
                    binary String (++) string string

                And ->
                    case l of
                        Bool True ->
                            Ok r

                        Bool False ->
                            Ok l

                        _ ->
                            stepLeft ()

                Or ->
                    case l of
                        Bool True ->
                            Ok l

                        Bool False ->
                            Ok r

                        _ ->
                            stepLeft ()

                Addition ->
                    case ( l, r ) of
                        ( Int li, Binary Addition (Int mi) rr ) ->
                            Ok (Binary Addition (Int (Int64.add li mi)) rr)

                        _ ->
                            binary Int Int64.add int int

                Subtraction ->
                    binary Int Int64.sub int int

                Multiplication ->
                    case int l of
                        Just v ->
                            case Int64.toInt53 v of
                                Just 0 ->
                                    Ok (Int v)

                                Just 1 ->
                                    Ok r

                                Just i ->
                                    if i == -1 then
                                        Ok (Unary Negation r)

                                    else
                                        binary Int Int64.mul int int

                                _ ->
                                    binary Int Int64.mul int int

                        Nothing ->
                            case int r of
                                Just v ->
                                    case Int64.toInt53 v of
                                        Just 0 ->
                                            Ok (Int v)

                                        Just 1 ->
                                            Ok l

                                        Just i ->
                                            if i == -1 then
                                                Ok (Unary Negation l)

                                            else
                                                binary Int Int64.mul int int

                                        _ ->
                                            binary Int Int64.mul int int

                                Nothing ->
                                    binary Int Int64.mul int int

                Division ->
                    binary Int Int64.div int int

                Modulo ->
                    binary Int (\lv rv -> lv |> Int64.remainderBy rv) int int

                LessThan ->
                    binary Bool Int64.lessThan int int

                GreaterThan ->
                    binary Bool Int64.moreThan int int

        Ternary c t f ->
            case c of
                Bool True ->
                    Ok t

                Bool False ->
                    Ok f

                _ ->
                    Result.map (\newC -> Ternary newC t f) (step c)

        Bool _ ->
            Ok icfp

        Int _ ->
            Ok icfp

        String _ ->
            Ok icfp

        Variable v ->
            Err v

        Lambda _ _ ->
            Ok icfp


reduced : Icfp -> Maybe Icfp
reduced icfp =
    if step icfp == Ok icfp then
        Just icfp

    else
        Nothing


lambda : Icfp -> Maybe ( UInt64, Icfp )
lambda icfp =
    case icfp of
        Lambda v b ->
            Just ( v, b )

        _ ->
            Nothing


int : Icfp -> Maybe Int64
int icfp =
    case icfp of
        Int i ->
            Just i

        Unary Negation (Int i) ->
            Just (Int64.negate i)

        _ ->
            Nothing


string : Icfp -> Maybe String
string icfp =
    case icfp of
        String s ->
            Just s

        _ ->
            Nothing


bool : Icfp -> Maybe Bool
bool icfp =
    case icfp of
        Bool b ->
            Just b

        _ ->
            Nothing


replace : ( UInt64, Icfp ) -> Icfp -> Icfp
replace ( var, expr ) val =
    let
        go : Icfp -> Icfp
        go child =
            case child of
                Variable v ->
                    if v == var then
                        val

                    else
                        child

                Unary op c ->
                    Unary op (go c)

                Binary op l r ->
                    Binary op (go l) (go r)

                Ternary c t f ->
                    Ternary (go c) (go t) (go f)

                Lambda v b ->
                    if v == var then
                        child

                    else
                        Lambda v (go b)

                _ ->
                    child
    in
    go expr


defaultBudget : number
defaultBudget =
    1000000


reduceWithBudget : Int -> Icfp -> Icfp
reduceWithBudget budget child =
    if budget <= 0 then
        child

    else
        case step child of
            Ok next ->
                if next == child then
                    child

                else
                    reduceWithBudget (budget - 1) next

            Err _ ->
                child
