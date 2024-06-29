module Roundtrip exposing (int, intRoundtrip)

import Expect
import Fuzz
import Icfp
import Test exposing (Test, describe, fuzz, test)


int : Test
int =
    describe "Int example"
        [ test "decodes" <|
            \_ ->
                "/6"
                    |> Icfp.decodeInt
                    |> Expect.equal 1337
        , test "encodes" <|
            \_ ->
                1337
                    |> Icfp.encodeInt
                    |> Expect.equal "/6"
        ]


intRoundtrip : Test
intRoundtrip =
    fuzz (Fuzz.intAtLeast 0) "Int roundtrips" <|
        \i ->
            i
                |> Icfp.encodeInt
                |> Icfp.decodeInt
                |> Expect.equal i
