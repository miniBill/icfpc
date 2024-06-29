module Icfp.ToElm exposing (toElm)

import Dict exposing (Dict)
import Elm
import Elm.Op
import Elm.ToString
import Icfp exposing (Binary(..), Icfp(..))
import Int64
import UInt64


toElm : Icfp -> String
toElm icfp =
    (toExpression Dict.empty icfp
        |> Elm.ToString.expression
    ).body


toExpression : Dict String Elm.Expression -> Icfp -> Elm.Expression
toExpression vars icfp =
    case icfp of
        Int i ->
            case Int64.toInt53 i of
                Just i53 ->
                    Elm.int i53

                Nothing ->
                    Elm.string (Int64.toString i ++ " cannot be represented as ain int53")

        Bool b ->
            Elm.bool b

        String s ->
            Elm.string s

        Binary op l r ->
            toBinary op
                (toExpression vars l)
                (toExpression vars r)

        Variable v ->
            Dict.get (UInt64.toString v) vars
                |> Maybe.withDefault (Elm.string "Variable not found")

        Lambda v b ->
            Elm.functionReduced "arg"
                (\arg ->
                    let
                        newVars : Dict String Elm.Expression
                        newVars =
                            Dict.insert (UInt64.toString v) arg vars
                    in
                    toExpression newVars b
                )

        Ternary c t f ->
            Elm.ifThen
                (toExpression vars c)
                (toExpression vars t)
                (toExpression vars f)

        _ ->
            Elm.string <| "ToElm.toElm - branch '" ++ Debug.toString icfp ++ "' not implemented"


toBinary : Binary -> Elm.Expression -> Elm.Expression -> Elm.Expression
toBinary op l r =
    case op of
        Addition ->
            Elm.Op.plus l r

        Subtraction ->
            Elm.Op.minus l r

        Multiplication ->
            Elm.Op.multiply l r

        Division ->
            Elm.Op.intDivide l r

        Modulo ->
            Elm.apply (Elm.val "modBy") [ l, r ]

        LessThan ->
            Elm.Op.lt l r

        GreaterThan ->
            Elm.Op.gt l r

        Equals ->
            Elm.Op.equal l r

        Or ->
            Elm.Op.or l r

        And ->
            Elm.Op.and l r

        Concat ->
            Elm.Op.append l r

        Take ->
            Elm.apply (Elm.val "String.left") [ l, r ]

        Drop ->
            Elm.apply (Elm.val "String.dropLeft") [ l, r ]

        CallByName ->
            Elm.apply l [ r ]

        CallLazy ->
            Elm.apply l [ r ]

        CallStrict ->
            Elm.apply l [ r ]
