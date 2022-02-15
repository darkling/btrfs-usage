port module Main exposing (main, upper_bound, min_arg, used_space, usage,
                           calc_unusable, location_to_model)

import Browser
import Html exposing (Html, h1, h2, button, div, span, text, label, input, br,
                      table, tr, th, td, a, p)
import Html.Attributes exposing (attribute, class, type_, name, value,
                                 checked, style, href, id, for)
import Html.Events exposing (onInput, onClick, onCheck)
import List.Extra
import Url
import Url.Builder as UB
import Url.Parser exposing ((<?>))
import Url.Parser as UP
import Url.Parser.Query as UPQ

main = Browser.document {
           init = init,
           view = view,
           update = update,
           subscriptions = subscriptions
       }

type Msg = AlterDeviceCount String
         | AlterDeviceSize Int String
         | AlterRaidParam RaidParamType String
         | SetRaidPreset RaidPreset
         | SetDegenKernel Bool
         | UrlChanged Model

type RaidPreset = Single
                | RAID0
                | RAID0d
                | RAID1
                | RAID1c3
                | RAID1c4
                | RAID10
                | RAID10d
                | RAID5
                | RAID6

type RaidParamType = Copies | MinStripes | MaxStripes | Parity
type alias RaidParams = { c: Int, slo: Int, shi: Int, p: Int }

type alias Model = {
        disk_size: List Int,
        raid_level: RaidParams,
        degen_kernel: Bool
    }

type alias Allocation = {
        usable: Int,
        stripe: Int,
        disks: List Int
    }

port pushUrl: String -> Cmd msg
port onUrlChange: (String -> msg) -> Sub msg

init: () -> (Model, Cmd Msg)
init _ = (
          {
              disk_size = [ 1000 ],
              raid_level = { c=1, slo=1, shi=1, p=0 },
              degen_kernel = True
          },
          Cmd.none
         )

-- Update

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AlterDeviceCount value ->
            let
                new_model = {model | disk_size = update_disk_list value model.disk_size}
            in
                (new_model, pushUrl <| build_url new_model)
        AlterDeviceSize dev value ->
            let
                new_model = {model | disk_size = update_disk_size dev value model.disk_size}
            in
                (new_model, pushUrl <| build_url new_model)
        AlterRaidParam which value ->
            let
                raid_level = update_raid_parameter which value model.raid_level
                new_model = {model | raid_level = raid_level}
            in
                (new_model, pushUrl <| build_url new_model)
        SetRaidPreset preset ->
            let
                new_model = {model | raid_level = raid_preset preset}
            in
                (new_model, pushUrl <| build_url new_model)
        SetDegenKernel value ->
            let
                new_model = {model | degen_kernel = value}
            in
                (new_model, pushUrl <| build_url new_model)
        UrlChanged new_model ->
            (new_model, Cmd.none)

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
                if raid_level.shi < value then
                    { raid_level | slo = value, shi = value }
                else
                    { raid_level | slo = value }
            MaxStripes ->
                if raid_level.slo > value then
                    { raid_level | slo = value, shi = value }
                else
                    { raid_level | shi = value }
            Parity ->
                { raid_level | p = value }


build_url model =
    UB.relative [] <| [ UB.int "c" model.raid_level.c,
                        UB.int "slo" model.raid_level.slo,
                        UB.int "shi" model.raid_level.shi,
                        UB.int "p" model.raid_level.p,
                        UB.int "dg" (if model.degen_kernel then 1 else 0)
                      ] ++ (List.map (UB.int "d") model.disk_size)

-- View

view model =
    let
        usage_values = usage model.raid_level model.disk_size
    in
    {
        title = "btrfs disk usage calculator",
        body = [
             h1 [] [ text "btrfs disk usage calculator" ],
             div [ class "main-section" ]
                 <| view_num_devices model.disk_size,
             div [ class "main-section" ]
                 [ h2 []  [ text "RAID levels" ],
                   div [ class "left-right-layout" ]
                       [ div [ class "raid-params" ]
                             <| view_raid_presets
                                  model.raid_level
                                  model.degen_kernel
                                  (List.length model.disk_size),
                         div [ class "detailed-params" ]
                         [ view_raid_params model.raid_level ],
                         div [ style "clear" "both" ] []
                       ]
                 ],
             div [ class "main-section" ]
                 [ h2 [] [ text "Device sizes" ],
                   view_devices model.disk_size usage_values,
                   view_usage_summary model usage_values,
                   view_stripe_key model.disk_size usage_values
                 ],
             div [ class "main-section" ]
                 [ p [] [
                       text "The results shown here do not take account of the space used by metadata, and are therefore approximate."
                       ],
                   p [] [
                       text "Please report any bugs found to the ",
                       a [ href "https://github.com/darkling/btrfs-usage/issues" ]
                       [ text "issue tracker" ],
                       text "."
                       ]
                 ]
            ]
    }

view_num_devices disks =
    [ label [ attribute "for" "num_disks" ] [ text "Number of devices: " ],
      input [ type_ "number",
              name "num_disks",
              attribute "min" "1",
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

view_raid_presets cur_level degen n =
    (view_raid_presets_list cur_level degen n)
        ++ [ input [ type_ "checkbox",
                     name "degen_kernel",
                     value <| string_from_bool degen,
                     onCheck SetDegenKernel
                   ] [],
             text "Kernel 5.15 or later"
           ]

view_raid_presets_list cur_level degen n =
    let
        items =
            case degen of
                False -> [Single, RAID0, RAID1, RAID1c3, RAID1c4, RAID10, RAID5, RAID6]
                True -> [Single, RAID0d, RAID1, RAID1c3, RAID1c4, RAID10d, RAID5, RAID6]
    in
        items
        |> List.map (raid_preset_line cur_level n)
        |> List.concat

raid_preset_line cur_level n preset =
    let
        text_name = raid_name preset
    in
        [ input [ type_ "radio",
                  name "raid-preset",
                  checked (is_raid_level cur_level n (raid_preset preset)),
                  value text_name,
                  id text_name,
                  onClick <| SetRaidPreset preset
                ] [],
          label [ for text_name ] [ text text_name ],
          br [] []
        ]

is_raid_level cur_level n that_level =
    cur_level.c == that_level.c
        && cur_level.p == that_level.p
        && cur_level.slo == that_level.slo
        && cur_level.shi == that_level.shi

view_devices: List Int -> List Allocation -> Html Msg
view_devices disks alloc =
    let
        bar_scale = toFloat <| Maybe.withDefault 1 <| List.maximum disks
        per_disk_stripes =
            case alloc of
                [] ->
                    List.repeat (List.length disks) []
                a ->
                    List.Extra.transpose <| List.map .disks a
        items =
            List.map2 Tuple.pair disks per_disk_stripes
                |> List.indexedMap (disk_to_device_line bar_scale)
                |> List.reverse
    in
        table [] items

disk_to_device_line bar_scale i (disk, stripes) =
    tr [] [ td [] [ input [ type_ "number",
                            class "disk-size",
                            name ("disk_size" ++ (String.fromInt i)),
                            attribute "min" "0",
                            value <| String.fromInt disk,
                            onInput <| AlterDeviceSize i
                          ] []
                  ],
            td [ class "full-width" ] <| device_usage_bar bar_scale disk stripes
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

max_bar = 99

bar_size scale alloc =
    String.fromFloat ((toFloat (alloc * max_bar)) / scale) ++ "%"

view_usage_summary: Model -> List Allocation -> Html Msg
view_usage_summary model usage_values =
    let
        raw_space = List.sum model.disk_size
        usable = usage_values
               |> List.map .usable
               |> List.sum
        unusable = calc_unusable model.disk_size usage_values
        ra = class "right-align"
    in
        table [] [ tr [] [ td [] [ text "Total space for files:" ],
                           td [ ra ] [ text <| String.fromInt usable ]
                         ],
                   tr [] [ td [] [ text "Total raw disk space:" ],
                           td [ ra ] [ text <| String.fromInt raw_space ]
                         ],
                   tr [] [ td [] [ text "Unusable:" ],
                           td [ ra ] [ text <| String.fromInt unusable ]
                         ]
                 ]

calc_unusable: List Int -> List Allocation -> Int
calc_unusable disks usage_values =
    let
        used = used_by_disk disks usage_values
        free = List.map2 (-) disks used
    in
        List.sum free

used_by_disk: List Int -> List Allocation -> List Int
used_by_disk disks usage_values =
    usage_values
        |> List.map .disks
        |> List.Extra.transpose
        |> List.map List.sum

view_stripe_key: List Int -> List Allocation -> Html Msg
view_stripe_key disks usage_values =
    let
        table_body = List.indexedMap stripe_key_column usage_values
        row_heads = [ th [ class "top-left-table" ] [ text "" ],
                      th [] [ text "Usable" ],
                      th [] [ text "Disks/alloc" ]
                    ]
        table_data = List.Extra.transpose (row_heads :: table_body)
    in
        table [ class "summary-table" ] <| List.map (tr []) table_data

stripe_key_column i { usable, stripe, disks } =
    let
        itxt = String.fromInt i
        stxt = String.fromInt <| modBy 4 i
        cls = class <| "stripe-" ++ stxt
    in
        [ th [ cls ] [ text <| "region " ++ itxt ],
          td [ cls ] [ text <| String.fromInt usable ],
          td [ cls ] [ text <| String.fromInt stripe ]
        ]

-- Subscriptions

subscriptions: Model -> Sub Msg
subscriptions model =
    onUrlChange (location_to_model >> UrlChanged)

location_to_model location =
    let
        raw_url = Url.fromString location
        url = Maybe.andThen (\u -> Just { u | path = "" }) raw_url
    in
        case Maybe.andThen (UP.parse query_to_model) url of
            Nothing ->
                Tuple.first <| init ()
            Just model ->
                if model.disk_size == [] then
                    { model | disk_size = [1000] }
                else
                    model

query_to_model =
    UP.query
        <| UPQ.map3 Model query_to_disks query_to_params query_to_dg

query_to_params =
    UPQ.map4 RaidParams
        (UPQ.map (Maybe.withDefault 1) (UPQ.int "c"))
        (UPQ.map (Maybe.withDefault 1) (UPQ.int "slo"))
        (UPQ.map (Maybe.withDefault 1) (UPQ.int "shi"))
        (UPQ.map (Maybe.withDefault 0) (UPQ.int "p"))

query_to_disks =
    UPQ.custom "d"
        (List.map String.toInt
             >> List.filter (\x -> case x of
                                       Nothing -> False
                                       Just _ -> True)
             >> List.map (\x -> case x of
                                    Nothing -> Debug.todo "should never happen"
                                    Just v -> v))

query_to_dg =
    UPQ.map parse_to_bool (UPQ.int "dg")

parse_to_bool x =
    Maybe.withDefault 1 x |> (==) 1

-- Model

usage: RaidParams -> List Int -> List Allocation
usage params input_disks =
    let
        return_value = upper_bound_general params input_disks
        { usable, disks } = return_value
        reduced_disks = List.map2 (-) input_disks disks
    in
        if usable == 0 then
            []
        else
            return_value :: usage params reduced_disks

upper_bound_general: RaidParams -> List Int -> Allocation
upper_bound_general params disks =
    let
        (o_disks, disk_order) = order_disks disks
        result = upper_bound params o_disks
    in
        { result | disks = unorder_disks disk_order result.disks }

upper_bound: RaidParams -> List Int -> Allocation
upper_bound params disks =
    -- Returns a structure:
    --   usable: usable space allocated,
    --   stripe: number of devices allocated on each pass,
    --   disks: list of space allocated on each disk
    let
        -- Number of disks total
        n_disks = List.length <| List.filter (\x -> x > 0) disks
        -- Largest disk
        max_disk = Maybe.withDefault -1 <| List.head disks
        -- Number of devices available to use
        avail_devs = min n_disks ((params.shi+params.p)*params.c)
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

order_disks disks =
    let
        enum_items = disks
              |> List.indexedMap Tuple.pair
              |> List.sortBy (Tuple.second >> negate)
    in
        (List.map Tuple.second enum_items,
         List.map Tuple.first enum_items)

unorder_disks orig_order disks =
    List.map2 Tuple.pair orig_order disks
        |> List.sortBy Tuple.first
        |> List.map Tuple.second

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

string_from_bool v =
    case v of
        True -> "true"
        False -> "false"

raid_preset: RaidPreset -> RaidParams
raid_preset preset =
    case preset of
        Single  -> { c=1, slo=1, shi=1,   p=0 }
        RAID0   -> { c=1, slo=2, shi=100, p=0 }
        RAID0d  -> { c=1, slo=1, shi=100, p=0 }
        RAID1   -> { c=2, slo=1, shi=1,   p=0 }
        RAID1c3 -> { c=3, slo=1, shi=1,   p=0 }
        RAID1c4 -> { c=4, slo=1, shi=1,   p=0 }
        RAID10  -> { c=2, slo=2, shi=100, p=0 }
        RAID10d -> { c=2, slo=1, shi=100, p=0 }
        RAID5   -> { c=1, slo=1, shi=100, p=1 }
        RAID6   -> { c=1, slo=1, shi=100, p=2 }

raid_name: RaidPreset -> String
raid_name preset =
    case preset of
        Single  -> "Single"
        RAID0   -> "RAID0"
        RAID0d  -> "RAID0"
        RAID1   -> "RAID1"
        RAID1c3 -> "RAID1c3"
        RAID1c4 -> "RAID1c4"
        RAID10  -> "RAID10"
        RAID10d -> "RAID10"
        RAID5   -> "RAID5"
        RAID6   -> "RAID6"
