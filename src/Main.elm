module Main exposing (main)

import Browser
import Html exposing (Html, h1, h2, button, div, text, label, input)
import Html.Attributes exposing (attribute, class, type_, name, value)

main = Browser.document {
           init = init,
           view = view,
           update = update,
           subscriptions = subscriptions
       }

type Msg = Increment String Int
         | Decrement String Int

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
    
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment item value ->
            (model, Cmd.none)
        Decrement item value ->
            (model, Cmd.none)

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
                 attribute "max" "100",
                 value <| String.fromInt <| List.length disks
               ] [
               ]
        ]

view_raid_levels preset =
    div [] []

view_devices disks =
    div [] []

-- Subscriptions

subscriptions: Model -> Sub msg
subscriptions model =
    Sub.none
