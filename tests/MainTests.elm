module MainTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main exposing (..)

suite : Test
suite =
    describe "main"
        [
         describe "upper_bound"
             [ test "Single should fill everything" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=1, p=0 }
                   in
                       upper_bound [25, 20, 15, 10] params

                   |> Expect.equal (70, [25, 20, 15, 10]),

               test "Too few disks (RAID-1) should use no space" <|
                   \_ ->
                   let
                       params = { c=3, slo=1, shi=1, p=0 }
                   in
                       upper_bound [25, 15] params

                   |> Expect.equal (0, [0, 0]),

               test "RAID-1 should fill the smaller device" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound [25, 15] params

                   |> Expect.equal (15, [15, 15])
             ],
         describe "min_arg"
             [ test "Should return the list element with the minimum value" <|
                   \_ ->
                   let
                       list = [(5, "a"), (1, "b"), (3, "c")]
                       pred = (\(i, c) -> i)
                   in
                       min_arg pred list

                   |> Expect.equal (Just (1, "b"))
             ]
        ]
