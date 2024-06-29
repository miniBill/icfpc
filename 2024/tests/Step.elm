module Step exposing (applyTest, intToString, modulo, stringToInt)

import Expect
import Icfp exposing (Icfp(..))
import Icfp.Step
import Test exposing (Test, describe, test)


modulo : Test
modulo =
    test "% (-3) 2 == -1" <|
        \_ ->
            -3
                |> remainderBy 2
                |> Expect.equal -1


stringToInt : Test
stringToInt =
    stepTestE "String to int" "U# S4%34" (Int 15818151)


intToString : Test
intToString =
    stepTestE "Int to string" "U$I4%34" (String "test")


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
                    icfp
                        |> Debug.log "parsed as"
                        |> Icfp.Step.step
                        |> Debug.log "stepped as "
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
