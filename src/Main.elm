module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)

main = Browser.document {
           init = init,
           view = view,
           update = update,
           subscriptions = subscriptions
       }

type Msg = Increment String Int
         | Decrement String Int

type alias Model = {
        num_disks: Int,
        disk_size: List Int
    }

init: () -> (Model, Cmd Msg)
init _ = (
          {
              num_disks = 1,
              disk_size = [ 1000 ]
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

view model =
    {
        title = "btrfs space tool",
        body = [ text "Hello, World!" ]
    }

subscriptions: Model -> Sub msg
subscriptions model =
    Sub.none
