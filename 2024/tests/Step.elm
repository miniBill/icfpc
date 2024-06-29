module Step exposing (applyTest, division, intToString, modulo, stringToInt)

import Expect
import Icfp exposing (Binary(..), Icfp(..), Unary(..))
import Icfp.Step
import Int64
import Test exposing (Test, describe, test)


modulo : Test
modulo =
    describe "modulo"
        [ test "% (-3) 2 == -1" <|
            \_ ->
                -3
                    |> remainderBy 2
                    |> Expect.equal -1
        , stepTestE "Modulo" "B% U- I$ I#" (Int (Int64.fromInt -1))
        ]


division : Test
division =
    stepTestEE "Division"
        (Binary
            Division
            (Unary Negation (Int (Int64.fromInt 3)))
            (Int (Int64.fromInt 2))
        )
        (Unary Negation (Int (Int64.fromInt 1)))


stringToInt : Test
stringToInt =
    stepTestE "String to int" "U# S4%34" (Int (Int64.fromInt 15818151))


intToString : Test
intToString =
    stepTestE "Int to string" "U$I4%34" (String "test")


stepTestEE : String -> Icfp -> Icfp -> Test
stepTestEE label from to =
    stepTest label (Icfp.toString from) (Icfp.toString to)


stepTestE : String -> String -> Icfp -> Test
stepTestE label from to =
    stepTest label from (Icfp.toString to)


stepTest : String -> String -> String -> Test
stepTest label from to =
    test label <|
        \_ ->
            case Icfp.parse from of
                Err _ ->
                    Expect.fail "Failed to parse"

                Ok icfp ->
                    let
                        _ =
                            Debug.log ("parsed as: " ++ Icfp.toString icfp) icfp

                        stepped =
                            icfp
                                |> Icfp.Step.step

                        _ =
                            Debug.log ("stepped as: " ++ Icfp.toString stepped) stepped
                    in
                    stepped
                        |> Icfp.toString
                        |> Expect.equal to


applyTest : Test
applyTest =
    describe "apply"
        [ stepTest
            "First"
            "B= B$ B$ B$ B$ L$ L$ L$ L# v$ I\" I# I$ I% I$ ? B= B$ L$ v$ I+ I+"
            "B= B$ B$ B$ L$ L$ L# v$ I# I$ I% I$"
        , stepTest
            "Second"
            "B= B$ B$ B$ L$ L$ L# v$ I# I$ I% I$"
            "B= B$ B$ L$ L# v$ I$ I% I$"
        , stepTest
            "Third"
            "B= B$ B$ L$ L# v$ I$ I% I$"
            "B= B$ L# I$ I% I$"
        , stepTest
            "Fourth"
            "B= B$ L# I$ I% I$"
            "B= I$ I$"
        ]
