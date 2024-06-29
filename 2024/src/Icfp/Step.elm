module Icfp.Step exposing (defaultBudget, reduce, reduceWithBudget, step)

import Icfp exposing (Binary(..), Icfp(..), Unary(..), decodeInt, decodeString, encodeInt, encodeString)
import Int64 exposing (Int64)
import UInt64 exposing (UInt64)


step : Icfp -> Icfp
step icfp =
    case icfp of
        Unary op c ->
            let
                unary resCtor toRes cparser =
                    case cparser c of
                        Just cvalue ->
                            resCtor (toRes cvalue)

                        Nothing ->
                            Unary op (step c)
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
                binary resCtor toRes lparser rparser =
                    case lparser l of
                        Just lvalue ->
                            case rparser r of
                                Just rvalue ->
                                    resCtor (toRes lvalue rvalue)

                                Nothing ->
                                    Binary op l (step r)

                        Nothing ->
                            Binary op (step l) r
            in
            case op of
                Equals ->
                    case int l of
                        Just li ->
                            case int r of
                                Just ri ->
                                    Bool (li == ri)

                                _ ->
                                    Binary Equals l (step r)

                        Nothing ->
                            case bool l of
                                Just lb ->
                                    case bool r of
                                        Just rb ->
                                            Bool (lb == rb)

                                        Nothing ->
                                            Binary Equals l (step r)

                                Nothing ->
                                    binary Bool (==) string string

                CallByName ->
                    binary identity replace lambda Just

                CallStrict ->
                    binary identity replace lambda reduced

                CallLazy ->
                    binary identity replace lambda Just

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
                            r

                        Bool False ->
                            l

                        _ ->
                            Binary And (step l) r

                Or ->
                    case l of
                        Bool True ->
                            l

                        Bool False ->
                            r

                        _ ->
                            Binary Or (step l) r

                Addition ->
                    case ( l, r ) of
                        ( Int li, Binary Addition (Int mi) rr ) ->
                            Binary Addition (Int (Int64.add li mi)) rr

                        _ ->
                            binary Int Int64.add int int

                Subtraction ->
                    binary Int Int64.sub int int

                Multiplication ->
                    case int l of
                        Just v ->
                            case Int64.toInt53 v of
                                Just 0 ->
                                    Int v

                                Just 1 ->
                                    r

                                Just i ->
                                    if i == -1 then
                                        Unary Negation r

                                    else
                                        binary Int Int64.mul int int

                                _ ->
                                    binary Int Int64.mul int int

                        Nothing ->
                            case int r of
                                Just v ->
                                    case Int64.toInt53 v of
                                        Just 0 ->
                                            Int v

                                        Just 1 ->
                                            l

                                        Just i ->
                                            if i == -1 then
                                                Unary Negation l

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
                    t

                Bool False ->
                    f

                _ ->
                    Ternary (step c) t f

        Bool _ ->
            icfp

        Int _ ->
            icfp

        String _ ->
            icfp

        Variable _ ->
            icfp

        Lambda _ _ ->
            icfp


reduced : Icfp -> Maybe Icfp
reduced icfp =
    if step icfp == icfp then
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


reduce : Icfp -> Icfp
reduce icfp =
    reduceWithBudget defaultBudget icfp


defaultBudget : number
defaultBudget =
    1000000


reduceWithBudget : Int -> Icfp -> Icfp
reduceWithBudget budget child =
    if budget <= 0 then
        child

    else
        let
            next : Icfp
            next =
                step child
        in
        if next == child then
            child

        else
            reduceWithBudget (budget - 1) next
