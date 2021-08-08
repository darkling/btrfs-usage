module Main exposing (main)

import Browser
import Html exposing (Html, h1, h2, button, div, span, text, label, input, br)
import Html.Attributes exposing (attribute, class, type_, name, value)
import Html.Events exposing (onInput)

main = Browser.document {
           init = init,
           view = view,
           update = update,
           subscriptions = subscriptions
       }

type Msg = AlterDeviceCount String

type RaidPreset = Single
                | RAID0
                | RAID1
                | RAID1c3
                | RAID1c4
                | RAID10
                | RAID5
                | RAID6

type alias Model = {
        disk_size: List Int,
        preset_selected: Maybe RaidPreset
    }

init: () -> (Model, Cmd Msg)
init _ = (
          {
              disk_size = [ 1000 ],
              preset_selected = Nothing
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
             Cmd.none)

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


-- View

view model =
    {
        title = "btrfs disk usage calculator",
        body = [
             h1 [] [ text "btrfs disk usage calcuator" ],
             view_num_devices model.disk_size,
             div [ class "main-section" ] [
                   h2 []  [ text "RAID levels" ],
                   div [ class "raid-levels" ] [
                         view_raid_levels model.preset_selected
                       ]
                 ],
             div [ class "main-section" ] [
                   h2 [] [ text "Device sizes" ],
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
               ] [
               ]
        ]

view_raid_levels preset =
    div [] []

view_devices disks =
    div [] <| List.map disk_to_device_line disks

disk_to_device_line disk =
    span [] [ text <| String.fromInt disk, br [] [] ]

-- Subscriptions

subscriptions: Model -> Sub msg
subscriptions model =
    Sub.none
