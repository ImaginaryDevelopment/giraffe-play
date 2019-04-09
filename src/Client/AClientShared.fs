module ClientShared

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fulma
open Fable.PowerPack


module KeyCode =
    let enter = 13.
let onKeyDown keyCode action =
    OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev
    )
module Controls =
    let dButton txt =
        Button.button
            [
                Button.IsFullWidth
                Button.Disabled true
            ]
            [str txt]

    let pButton txt onClick =
        Button.button
            [ Button.IsFullWidth
              Button.Color IsPrimary
              Button.OnClick onClick ]
            [ str txt ]
module Storage =
    open Thoth.Json
    // concern: how does this handle the key being present but no value?
    let inline get<'t> key :'t option =
        match BrowserLocalStorage.load (Decode.Auto.generateDecoder<'t>()) key with
        | Ok (x:'t) ->
            Some x
        | Error e ->
            eprintfn "Local storage issue:%s" e
            None
    let save<'t> key (value:'t) =
        BrowserLocalStorage.save key value
    let delete key =
        BrowserLocalStorage.delete key
    type IStore<'t> =
        abstract member Get: unit -> 't option
        abstract member Set: 't -> unit
        abstract member Delete:unit -> unit
    let inline store<'t>(key) =
        {new IStore<'t> with
            member __.Get() = get<'t> key
            member __.Set v = save<'t> key v
            member __.Delete () = delete key
        }


