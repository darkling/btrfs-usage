module Main exposing (main)

import Browser
import Html exposing (Html, h1, h2, button, div, span, text, label, input, br,
                      table, tr, td)
import Html.Attributes exposing (attribute, class, type_, name, value, checked)
import Html.Events exposing (onInput, onClick)

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
             view_num_devices model.disk_size,
             div [ class "main-section" ]
                 [ h2 []  [ text "RAID levels" ],
                   div [ class "raid-params" ]
                       [ view_raid_params model.raid_level ]
                 ],
             div [ class "main-section" ]
                 [ h2 [] [ text "Device sizes" ],
                   view_devices model.disk_size
                 ]
            ]
    }

view_num_devices disks =
    div [] [
         label [ attribute "for" "num_disks" ] [ text "Number of devices: " ],
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

view_devices disks =
    div [] <| List.reverse <| List.indexedMap disk_to_device_line disks

disk_to_device_line i disk =
    span [] [ input [ type_ "number",
                      name ("disk_size" ++ (String.fromInt i)),
                      attribute "min" "0",
                      value <| String.fromInt disk,
                      onInput <| AlterDeviceSize i
                    ] [],
              br [] []
            ]

-- Subscriptions

subscriptions: Model -> Sub msg
subscriptions model =
    Sub.none

-- Model

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
