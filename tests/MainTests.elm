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
                       upper_bound params [25, 20, 15, 10]

                   |> Expect.equal (70, [25, 20, 15, 10]),

               test "Too few disks (RAID-1) should use no space" <|
                   \_ ->
                   let
                       params = { c=3, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 15]

                   |> Expect.equal (0, [0, 0]),

               test "RAID-1 should fill the smaller device of two" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 15]

                   |> Expect.equal (15, [15, 15]),

               test "RAID-1 should fill all devices, if approximately even" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [24, 15, 15]

                   |> Expect.equal (27, [24, 15, 15]),

               test "RAID-1 should fill almost all devices, if approximately even" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 15, 15]

                   |> Expect.equal (27, [24, 15, 15]),

               test "RAID-1 should fill smaller devices, if uneven" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 10, 10]

                   |> Expect.equal (20, [20, 10, 10]),

               test "RAID-1c3 should fill from the left" <|
                   \_ ->
                   let
                       params = { c=3, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [4, 4, 4, 4]

                   |> Expect.equal (5, [4, 4, 4, 3]),

               test "RAID-0 should fill the smallest device only" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=100, p=0 }
                   in
                       upper_bound params [25, 15, 15]

                   |> Expect.equal (45, [15, 15, 15]),

               test "Bounded RAID-0 should fill more devices" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=3, p=0 }
                   in
                       upper_bound params [25, 15, 15, 10]

                   |> Expect.equal (60, [20, 15, 15, 10]),

               test "RAID-10 should fill almost everything" <|
                   \_ ->
                   let
                       params = { c=2, slo=2, shi=2, p=0 }
                   in
                       upper_bound params [6, 5, 4, 4, 4]

                   |> Expect.equal (10, [4, 4, 4, 4, 4]),

               test "RAID-5 should allocate all the devices" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=5, p=1 }
                   in
                       upper_bound params [25, 15, 15, 10]

                   |> Expect.equal (30, [10, 10, 10, 10])
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
             ],
         describe "used_space"
             [ test "Should handle an even spread of full disks" <|
                   \_ ->
                   used_space 9 [3, 3, 3]
                   |> Expect.equal [3, 3, 3],

               test "Should handle an even spread of partially-full disks" <|
                   \_ ->
                   used_space 9 [4, 4, 4]
                   |> Expect.equal [3, 3, 3],

               test "Should fill a nearly even spread from the right" <|
                   \_ ->
                   used_space 14 [4, 4, 4, 4]
                   |> Expect.equal [4, 4, 3, 3],

               test "Should fill unequal devices from the right" <|
                   \_ ->
                   used_space 6 [4, 2, 1]
                   |> Expect.equal [3, 2, 1]
             ],
         describe "global_usage"
             [ test "Should have one group from single" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=1, p=0 }
                   in
                       usage params [25, 10, 5]

                   |> Expect.equal [(40, [25, 10, 5])],

               test "Should have one group from RAID-1" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       usage params [25, 10, 5]

                   |> Expect.equal [(15, [15, 10, 5])],

               test "Should be able to get multiple groups from RAID-0" <|
                   \_ ->
                   let
                       params = { c=1, slo=2, shi=10, p=0 }
                   in
                       usage params [4, 2, 1, 1]

                   |> Expect.equal [(4, [1, 1, 1, 1]), (2, [1, 1])]
             ]
        ]
