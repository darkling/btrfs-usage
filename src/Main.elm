module Main exposing (main, upper_bound, min_arg, used_space, usage, usage_ordered)

import Browser
import Html exposing (Html, h1, h2, button, div, span, text, label, input, br,
                      table, tr, td)
import Html.Attributes exposing (attribute, class, type_, name, value,
                                 checked, style)
import Html.Events exposing (onInput, onClick)
import List.Extra

main = Browser.document {
           init = init,
           view = view,
           update = update,
           subscriptions = subscriptions
       }

type Msg = AlterDeviceCount String
         | AlterDeviceSize Int String
         | AlterRaidParam RaidParamType String

type RaidPreset = Single
                | RAID0
                | RAID1
                | RAID1c3
                | RAID1c4
                | RAID10
                | RAID5
                | RAID6

type RaidParamType = Copies | MinStripes | MaxStripes | Parity
type alias RaidParams = { c: Int, slo: Int, shi: Int, p: Int }

type alias Model = {
        disk_size: List Int,
        raid_level: RaidParams
    }

type alias Allocation = {
        usable: Int,
        stripe: Int,
        disks: List Int
    }

init: () -> (Model, Cmd Msg)
init _ = (
          {
              disk_size = [ 1000 ],
              raid_level = { c=1, slo=1, shi=1, p=0 }
          },
          Cmd.none
         )

-- Update
    
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AlterDeviceCount value ->
            ({model | disk_size = update_disk_list value model.disk_size},
             Cmd.none)
        AlterDeviceSize dev value ->
            ({model | disk_size = update_disk_size dev value model.disk_size},
             Cmd.none)
        AlterRaidParam which value ->
            let
                raid_level = update_raid_parameter which value model.raid_level
            in
                ({model | raid_level = raid_level}, Cmd.none)

update_disk_list: String -> List Int -> List Int
update_disk_list text_value disks =
    let
        value = Maybe.withDefault 1 (String.toInt text_value)
    in
        if value == List.length disks then
            disks
        else if value < List.length disks then
                 case List.tail disks of
                     Nothing ->
                         update_disk_list text_value []
                     Just tail ->
                         update_disk_list text_value tail
             else -- if value > List.length disks then
                 case List.head disks of
                     Nothing ->
                         update_disk_list text_value (1000 :: disks)
                     Just size ->
                         update_disk_list text_value (size :: disks)

update_disk_size: Int -> String -> List Int -> List Int
update_disk_size index text_value disks =
    let
        value = Maybe.withDefault 1 (String.toInt text_value)
    in
        List.indexedMap (update_one_disk_size index value) disks

update_one_disk_size: Int -> Int -> Int -> Int -> Int
update_one_disk_size seek value index disk =
    if seek == index then
        value
    else
        disk

update_raid_parameter param_type text_value raid_level =
    let
        value = Maybe.withDefault 1 (String.toInt text_value)
    in
        case param_type of
            Copies ->
                { raid_level | c = value }
            MinStripes ->
                { raid_level | slo = value }
            MaxStripes ->
                { raid_level | shi = value }
            Parity ->
                { raid_level | p = value }

-- View

view model =
    {
        title = "btrfs disk usage calculator",
        body = [
             h1 [] [ text "btrfs disk usage calcuator" ],
             div [ class "main-section" ]
                 <| view_num_devices model.disk_size,
             div [ class "main-section" ]
                 [ h2 []  [ text "RAID levels" ],
                   div [ class "raid-params" ]
                       [ view_raid_params model.raid_level ]
                 ],
             div [ class "main-section" ]
                 [ h2 [] [ text "Device sizes" ],
                   view_devices model.disk_size
                       <| usage model.raid_level model.disk_size
                 ]
            ]
    }

view_num_devices disks =
    [ label [ attribute "for" "num_disks" ] [ text "Number of devices: " ],
      input [ type_ "number",
              name "num_disks",
              attribute "min" "0",
              attribute "max" "100",
              value <| String.fromInt <| List.length disks,
              onInput AlterDeviceCount
            ] []
    ]

view_raid_params level =
    div [] [ table [] [
                  raid_param_line "Copies:" "c" level.c Copies,
                  raid_param_line "Min data stripes:" "slo" level.slo MinStripes,
                  raid_param_line "Max data stripes:" "shi" level.shi MaxStripes,
                  raid_param_line "Parity stripes:" "p" level.p Parity
                 ]
        ]

raid_param_line label ctrl_name param_value event =
    tr [] [ td [] [ text label ],
            td [] [ input [ type_ "number",
                            name ctrl_name,
                            attribute "min" "0",
                            attribute "max" "100",
                            value <| String.fromInt param_value,
                            onInput <| AlterRaidParam event
                        ] []
                ]
        ]

view_devices: List Int -> List Allocation -> Html Msg
view_devices disks alloc =
    let
        bar_scale = Maybe.withDefault 1 <| List.maximum disks
        per_disk_stripes = List.Extra.transpose <| List.map .disks alloc
        items =
            List.map2 Tuple.pair disks per_disk_stripes
                |> List.indexedMap (disk_to_device_line bar_scale)
                |> List.reverse
    in
        table [ class "" ] items

disk_to_device_line bar_scale i (disk, stripes) =
    tr [] [ td [] [ input [ type_ "number",
                            class "disk-size",
                            name ("disk_size" ++ (String.fromInt i)),
                            attribute "min" "0",
                            value <| String.fromInt disk,
                            onInput <| AlterDeviceSize i
                          ] []
                  ],
            td [] <| device_usage_bar bar_scale disk stripes
          ]

device_usage_bar bar_scale disk stripes =
    let
        used = List.map
               (\alloc -> div [ class "usage-bar",
                                    style "width" <| bar_size bar_scale alloc
                              ] []
               )
               stripes
        all_used = List.sum(stripes)
    in
        if disk > all_used then
            used ++ [div [ class "usage-bar",
                           class "empty",
                           style "width" <| bar_size bar_scale (disk-all_used)
                         ] []
                    ]
        else
            used

max_bar = 800

bar_size scale alloc =
    String.fromInt (alloc * max_bar // scale) ++ "px"

-- Subscriptions

subscriptions: Model -> Sub msg
subscriptions model =
    Sub.none

-- Model

usage: RaidParams -> List Int -> List Allocation
usage params disks =
    let
        id_map = disks
                 |> List.indexedMap Tuple.pair
                 |> List.sortBy (\t -> negate <| Tuple.second t)
        perm = List.map Tuple.first id_map
        ord_disks = List.map Tuple.second id_map
    in
        usage_ordered params ord_disks
            |> List.map (reorder_disks perm)

reorder_disks: List Int -> Allocation -> Allocation
reorder_disks perm alloc =
    { alloc |
      disks = alloc.disks
          |> List.map2 Tuple.pair perm
          |> List.sort
          |> List.map Tuple.second
    }

usage_ordered: RaidParams -> List Int -> List Allocation
usage_ordered params disks =
    usage_impl params disks

usage_impl: RaidParams -> List Int -> List Allocation
usage_impl params input_disks =
    let
        return_value = upper_bound params input_disks
        { usable, disks } = return_value
        reduced_disks = List.map2 (-) input_disks disks
    in
        if usable == 0 then
            []
        else
            return_value :: usage_impl params reduced_disks

upper_bound: RaidParams -> List Int -> Allocation
upper_bound params disks =
    -- Returns a 2-tuple:
    --   _usable_ space allocated,
    --   list of space allocated on each disk
    let
        -- Number of disks total
        n_disks = List.length <| List.filter (\x -> x > 0) disks
        -- Largest disk
        max_disk = Maybe.withDefault -1 <| List.head disks
        -- Number of devices available to use
        avail_devs = min n_disks (params.shi+params.p)*params.c
        -- Numer of devices actually used (round down to a multiple of copies)
        stripe = avail_devs - modBy params.c avail_devs
        -- Trivial bound
        trivial_bound = (List.sum disks) // stripe
        -- Possible values for other bounds
        maybe_bounds = bounds_by_disk stripe disks
        -- Actual bounds
        bounds = List.filter (\(i, d, b) -> d >= b) maybe_bounds
        -- Smallest upper bound
        actual_bound = min_arg (\(i, d, b) -> b)
                       <| (-1, max_disk, trivial_bound) :: bounds
        -- Get the bounding disk index and bound size
        (idx, _, bd) = Maybe.withDefault (-1, -1, -1) actual_bound
        -- Available space
        space_available = bd * ((stripe // params.c) - params.p)
    in
        if n_disks < (params.slo+params.p)*params.c then
            -- Not enough disks: nothing to do
            { usable=0,
              stripe=stripe,
              disks=List.repeat n_disks 0
            }
        else
            { usable=space_available,
              stripe=stripe,
              disks=used_space (bd*stripe) disks
            }

bounds_by_disk: Int -> List Int -> List (Int, Int, Int)
bounds_by_disk stripe disks = bounds_by_disk_impl stripe 1 disks

bounds_by_disk_impl stripe idx disk_list =
    case disk_list of
        [] ->
            []
        [disk] ->
            []
        (disk :: disks) ->
            let
                bound = (List.sum disks) // (stripe - idx)
            in
                if stripe <= idx then
                    []
                else
                    (idx, disk, bound) :: bounds_by_disk_impl stripe (idx+1) disks

used_space: Int -> List Int -> List Int
used_space remaining disks =
    disks
        |> List.indexedMap Tuple.pair
        |> List.foldr used_space_fold ([], remaining)
        |> Tuple.first

used_space_fold: (Int, Int) -> (List Int, Int) -> (List Int, Int)
used_space_fold (i, disk) (res, remaining) =
    let
        average_fill = remaining // (i+1)
        this_fill = min average_fill disk
    in
        (this_fill :: res,
         remaining - this_fill
        )

min_arg: (a -> Int) -> List a -> Maybe a
min_arg pred list =
    case list of
        [] -> Nothing
        [item] ->
            Just item
        (head :: tail) ->
            let
                min_rest_maybe = min_arg pred tail
            in
                case min_rest_maybe of
                    Nothing ->
                        Just head
                    Just min_rest ->
                        if pred head < pred min_rest then
                            Just head
                        else
                            Just min_rest

raid_presets: RaidPreset -> RaidParams
raid_presets preset =
    case preset of
        Single  -> { c=1, slo=1, shi=1,   p=0 }
        RAID0   -> { c=1, slo=2, shi=100, p=0 }
        RAID1   -> { c=2, slo=1, shi=1,   p=0 }
        RAID1c3 -> { c=3, slo=1, shi=1,   p=0 }
        RAID1c4 -> { c=4, slo=1, shi=1,   p=0 }
        RAID10  -> { c=2, slo=2, shi=100, p=0 }
        RAID5   -> { c=1, slo=1, shi=100, p=1 }
        RAID6   -> { c=1, slo=1, shi=100, p=2 }
