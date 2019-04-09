module ClientShared

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fulma

module KeyCode =
    let enter = 13.
let onKeyDown keyCode action =
    OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev
    )
module Controls =
    let pButton txt onClick =
        Button.button
            [ Button.IsFullWidth
              Button.Color IsPrimary
              Button.OnClick onClick ]
            [ str txt ]