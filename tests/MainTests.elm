module MainTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main exposing (..)

suite : Test
suite =
    describe "upper_bound"
        [ test "Single should fill everything" <|
              \_ ->
              let
                  params = { c=1, slo=1, shi=1, p=0 }
              in
                  upper_bound [25, 20, 15, 10] params

              |> Expect.equal (-1, -1, [25, 20, 15, 10]),

          test "RAID-1 should fill the smaller device" <|
              \_ ->
              let
                  params = { c=2, slo=1, shi=1, p=0 }
              in
                  upper_bound [25, 15] params

              |> Expect.equal (-1, -1, [15, 15])
        ]
