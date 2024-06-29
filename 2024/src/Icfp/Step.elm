module Icfp.Step exposing (reduce, step)

import Icfp exposing (Binary(..), Icfp(..), Unary(..), decodeInt, decodeString, encodeInt, encodeString)


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
                    unary never never (\_ -> Nothing)

                IntToString ->
                    unary String (encodeInt >> decodeString) int

                StringToInt ->
                    unary Int (encodeString >> decodeInt) string

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

                Apply ->
                    binary identity replace lambda Just

                Drop ->
                    binary String String.dropLeft int string

                Take ->
                    binary String String.left int string

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
                    binary Int (+) int int

                Subtraction ->
                    binary Int (-) int int

                Multiplication ->
                    binary Int (*) int int

                Division ->
                    binary Int (//) int int

                Modulo ->
                    binary Int (\lv rv -> lv |> remainderBy rv) int int

                LessThan ->
                    binary Bool (<) int int

                GreaterThan ->
                    binary Bool (>) int int

        Ternary c t f ->
            case c of
                Bool True ->
                    t

                Bool False ->
                    f

                _ ->
                    Ternary (step c) t f

        _ ->
            icfp


lambda : Icfp -> Maybe ( Int, Icfp )
lambda icfp =
    case icfp of
        Lambda v b ->
            Just ( v, b )

        _ ->
            Nothing


int : Icfp -> Maybe Int
int icfp =
    case icfp of
        Int i ->
            Just i

        Unary Negation (Int i) ->
            Just -i

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


replace : ( Int, Icfp ) -> Icfp -> Icfp
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
    let
        go budget child =
            if budget <= 0 then
                child

            else
                let
                    next =
                        step child
                in
                if next == child then
                    child

                else
                    go (budget - 1) next
    in
    go 1000000 icfp
