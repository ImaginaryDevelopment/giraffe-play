namespace Porter

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
open ClientShared

type Msg =
    | PortChanged of string
    | PortCheck
    | PortCheckLoaded of Result<PingStatus,exn>

type Model = {PortInput:int option; PingStatus:PingStatus option;ValidationError:string option}

module PorterImpl =
    open Microsoft.FSharp.Core
    let listenerCheck port =
        fetchAs<PingStatus> (sprintf "/api/ping?port=%i" port) (Decode.Auto.generateDecoder())
    //consider loading up from localstorage any saved ports and listening for them
    let init () : Model * Cmd<Msg> =
        {PortInput = None;PingStatus=None;ValidationError = None},Cmd.none


    let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
        match msg with
        | PortChanged raw ->
            match System.Int32.TryParse(raw) with
            | true, x ->
                {currentModel with PortInput = Some x;ValidationError=None}, Cmd.none
            | false, _ -> {currentModel with ValidationError= Some "Failed to read port"}, Cmd.none
        | PortCheck ->
            match currentModel.PortInput with
            | None ->
                {currentModel with ValidationError=Some "No valid port to check"}, Cmd.none
            | Some port ->
                let cmd = Cmd.ofPromise (listenerCheck port) [] (Ok >> PortCheckLoaded) (Microsoft.FSharp.Core.Error >> PortCheckLoaded)
                {currentModel with ValidationError = None}, cmd
        | PortCheckLoaded (Ok ps) ->
            {currentModel with PingStatus = Some ps}, Cmd.none
        | PortCheckLoaded (Error ex) ->
            {currentModel with ValidationError = Some <| sprintf "%A" ex}, Cmd.none
module Run =
    let view (model : Model) (dispatch : Msg -> unit) =

      Container.container [][
        Label.label [] [str "Enter port to check"]
        Input.number [Input.Placeholder "Ex: 8080"
                      Input.Value (string model.PortInput)
                    //   Input.Modifiers [ Modifier.TextTransform TextTransform.UpperCase ]
                      Input.Color (if model.ValidationError.IsSome then Color.IsDanger else Color.IsSuccess)
                      Input.Props [ OnChange (fun ev -> dispatch (PortChanged !!ev.target?value)); onKeyDown KeyCode.enter (fun _ -> dispatch PortCheck) ] ]
        Label.label [] [ model.ValidationError |> Option.defaultValue null |> str ]
        Label.label [] [ model.PingStatus |> Option.map (fun ps -> sprintf "Port %i - %s" ps.Port ps.PortStatus) |> Option.defaultValue "" |> str]
      ]


