[<RequireQualifiedAccess>]
module CounterApp

open Thoth.Json
open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.JS
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Fulma

open Shared
open ClientShared.Controls

type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Result<Counter, exn>
type Model = {Counter:Counter option;Message:string option; LoadError:string option} with
    static member Zero = {Counter=None;Message=None;LoadError=None}

open Microsoft.FSharp.Core
let initialCounter = fetchAs<Counter> "/api/init" (Decode.Auto.generateDecoder())
//consider loading up from localstorage any saved ports and listening for them
let init () : Model * Cmd<Msg> =

    let loadCountCmd =
        Cmd.ofPromise
            initialCounter
            []
            (Ok >> InitialCountLoaded)
            (Error >> InitialCountLoaded)
    let initialModel = {Counter=None;Message=Some "Loading";LoadError=None}
    initialModel, loadCountCmd


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some counter, Increment ->
        let nextModel = {Model.Zero with Counter=Some { Value = counter.Value + 1 }}
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel = {Model.Zero with Counter=Some { Value = counter.Value - 1 }}
        nextModel, Cmd.none
    | _, InitialCountLoaded (Ok initialCount) ->
        let nextModel = {Model.Zero with Counter=Some initialCount}
        nextModel, Cmd.none
    | _, InitialCountLoaded (Error e)->
        let nextModel = {Model.Zero with LoadError=Some e.Message}
        nextModel, Cmd.none


module Components =
    let show = function
    | Some counter  -> string counter.Value
    | None -> "Loading..."
open Components
let view (model : Model) (dispatch : Msg -> unit) =
  Container.container []
      [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model.Counter) ] ]
        Columns.columns []
            [ Column.column [] [ pButton "-" (fun _ -> dispatch Decrement) ]
              Column.column [] [ pButton "+" (fun _ -> dispatch Increment) ] ]
      ]


