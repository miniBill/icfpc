module Roundtrip exposing (int, intRoundtrip)

import Expect
import Fuzz
import Icfp
import Test exposing (Test, describe, fuzz, test)
import UInt64


int : Test
int =
    describe "Int example"
        [ test "decodes" <|
            \_ ->
                "/6"
                    |> Icfp.decodeInt
                    |> Expect.equal (UInt64.fromInt 1337)
        , test "encodes" <|
            \_ ->
                UInt64.fromInt 1337
                    |> Icfp.encodeInt
                    |> Expect.equal "/6"
        ]


intRoundtrip : Test
intRoundtrip =
    fuzz (Fuzz.map UInt64.fromInt <| Fuzz.intAtLeast 0) "Int roundtrips" <|
        \i ->
            i
                |> Icfp.encodeInt
                |> Icfp.decodeInt
                |> Expect.equal i
