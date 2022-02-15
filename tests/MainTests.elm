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

                   |> Expect.equal { usable=70,
                                     stripe=1,
                                     disks=[25, 20, 15, 10]
                                   },

               test "Too few disks (RAID-1c3) should use no space" <|
                   \_ ->
                   let
                       params = { c=3, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 15]

                   |> Expect.equal { usable=0,
                                     stripe=0,
                                     disks=[0, 0]
                                   },

               test "RAID-1 should fill the smaller device of two" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 15]

                   |> Expect.equal { usable=15,
                                     stripe=2,
                                     disks=[15, 15]
                                   },

               test "RAID-1 should fill all devices, if approximately even" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [24, 15, 15]

                   |> Expect.equal { usable=27,
                                     stripe=2,
                                     disks=[24, 15, 15]
                                   },

               test "RAID-1 should fill almost all devices, if approximately even" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 15, 15]

                   |> Expect.equal { usable=27,
                                     stripe=2,
                                     disks=[24, 15, 15]
                                   },

               test "RAID-1 should fill smaller devices, if uneven" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [25, 10, 10]

                   |> Expect.equal { usable=20,
                                     stripe=2,
                                     disks=[20, 10, 10]
                                   },

               test "RAID-1c3 should fill from the left" <|
                   \_ ->
                   let
                       params = { c=3, slo=1, shi=1, p=0 }
                   in
                       upper_bound params [4, 4, 4, 4]

                   |> Expect.equal { usable=5,
                                     stripe=3,
                                     disks=[4, 4, 4, 3] },

               test "RAID-0 should fill the smallest device only" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=100, p=0 }
                   in
                       upper_bound params [25, 15, 15]

                   |> Expect.equal { usable=45,
                                     stripe=3,
                                     disks=[15, 15, 15] },

               test "Bounded RAID-0 should fill more devices" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=3, p=0 }
                   in
                       upper_bound params [25, 15, 15, 10]

                   |> Expect.equal { usable=60,
                                     stripe=3,
                                     disks=[20, 15, 15, 10] },

               test "RAID-10 should fill almost everything" <|
                   \_ ->
                   let
                       params = { c=2, slo=2, shi=2, p=0 }
                   in
                       upper_bound params [6, 5, 4, 4, 4]

                   |> Expect.equal { usable=10,
                                     stripe=4,
                                     disks=[4, 4, 4, 4, 4]
                                   },

               test "RAID-1/10 should fill almost everything" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=2, p=0 }
                   in
                       upper_bound params [10, 10, 10, 10, 10]
                   |> Expect.equal { usable=24,
                                     stripe=4,
                                     disks=[10, 10, 10, 9, 9]
                                   },

               test "RAID-5 should allocate all the devices" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=5, p=1 }
                   in
                       upper_bound params [25, 15, 15, 10]

                   |> Expect.equal { usable=30,
                                     stripe=4,
                                     disks=[10, 10, 10, 10]
                                   }
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
         describe "usage"
             [ test "Should have one group from single" <|
                   \_ ->
                   let
                       params = { c=1, slo=1, shi=1, p=0 }
                   in
                       usage params [25, 10, 5]

                   |> Expect.equal [{ usable=40, stripe=1, disks=[25, 10, 5] }],

               test "Should have one group from RAID-1" <|
                   \_ ->
                   let
                       params = { c=2, slo=1, shi=1, p=0 }
                   in
                       usage params [25, 10, 5]

                   |> Expect.equal [{ usable=15, stripe=2, disks=[15, 10, 5] }],

               test "Should be able to get multiple groups from RAID-0" <|
                   \_ ->
                   let
                       params = { c=1, slo=2, shi=10, p=0 }
                   in
                       usage params [4, 2, 1, 1]

                   |> Expect.equal [{ usable=4, stripe=4, disks=[1, 1, 1, 1] },
                                    { usable=2, stripe=2, disks=[1, 1, 0, 0] }],

               test "Should work with unordered lists" <|
                   \_ ->
                   let
                       params = { c=1, slo=2, shi=10, p=0 }
                   in
                       usage params [1, 1, 4, 2]

                   |> Expect.equal [{ usable=4, stripe=4, disks=[1, 1, 1, 1] },
                                    { usable=2, stripe=2, disks=[0, 0, 1, 1] }],

             test "Should work with unordered lists (part 2)" <|
                 \_ ->
                 let
                     params = { c=2, slo=1, shi=1, p=0 }
                     disks = [1000, 1000, 1000, 1000, 4000]
                 in
                     usage params disks
                 |> Expect.equal [{ usable=4000,
                                    stripe=2,
                                    disks=[1000, 1000, 1000, 1000, 4000]} ],

             test "Should handle odd devices, variable stripes equally (max 2 vs max 4)" <|
                 \_ ->
                 let
                     params2 = { c=2, slo=1, shi=2, p=0 }
                     params4 = { c=2, slo=1, shi=4, p=0 }
                     disks = [10, 10, 10, 10, 10]
                 in
                     Expect.equal
                         (usage params2 disks)
                         (usage params4 disks),

             test "Should handle odd devices, variable stripes equally (max 2 vs max 3)" <|
                 \_ ->
                 let
                     params2 = { c=2, slo=1, shi=2, p=0 }
                     params3 = { c=2, slo=1, shi=3, p=0 }
                     disks = [10, 10, 10, 10, 10]
                 in
                     Expect.equal
                         (usage params2 disks)
                         (usage params3 disks),

             test "Should handle wider stripe than devices" <|
                 \_ ->
                 let
                     params = { c=4, slo=2, shi=2, p=0 }
                     disks = [10, 10, 10]
                 in
                     usage params disks
                 |> Expect.equal []
             ],
         describe "calc_unusable"
             [ test "Should handle full disks" <|
                   \_ ->
                   let
                       disks = [4, 3, 2, 1]
                       usage = [{ usable=10, stripe=1, disks=[4, 3, 2, 1] }]
                   in
                       calc_unusable disks usage
                   |> Expect.equal 0,

               test "Should handle partially empty disks" <|
                   \_ ->
                   let
                       disks = [4, 3, 2, 1]
                       usage = [{ usable=7, stripe=1, disks=[2, 2, 2, 1] }]
                   in
                       calc_unusable disks usage
                   |> Expect.equal 3,

               test "Should handle multiple records" <|
                   \_ ->
                   let
                       disks = [4, 3, 2, 1]
                       usage = [{ usable=7, stripe=1, disks=[2, 2, 2, 1] },
                                { usable=2, stripe=1, disks=[1, 1, 0, 0] }]
                   in
                       calc_unusable disks usage
                   |> Expect.equal 1
             ],
         describe "url parsing"
             [ test "Should parse a full URL with no path" <|
                   \_ ->
                   let
                       url = "https://localhost/?c=1&slo=1&shi=4&p=1&d=1000"
                   in
                       location_to_model url
                   |> Expect.equal { disk_size=[1000],
                                     raid_level={ c=1, slo=1, shi=4, p=1 },
                                     degen_kernel=True
                                   },
               test "Should parse a full URL with a one-element path" <|
                   \_ ->
                   let
                       url = "https://localhost/here/?c=1&slo=1&shi=4&p=1&d=1000"
                   in
                       location_to_model url
                   |> Expect.equal { disk_size=[1000],
                                     raid_level={ c=1, slo=1, shi=4, p=1 },
                                     degen_kernel=True
                                   },
               test "Should parse a full URL with a two-element path" <|
                   \_ ->
                   let
                       url = "https://localhost/here/there/?c=1&slo=1&shi=4&p=1&d=1000"
                   in
                       location_to_model url
                   |> Expect.equal { disk_size=[1000],
                                     raid_level={ c=1, slo=1, shi=4, p=1 },
                                     degen_kernel=True
                                   }
             ]
        ]
