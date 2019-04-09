namespace Porter

open Thoth.Json
open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Import

open Fulma

open Shared
open Shared.StringPatterns

open ClientShared
open System.Threading.Tasks

type Name = string
type Port = {Name:Name;Port:int}

type Msg =
    | PortNamed of string
    | PortChanged of string
    | PortCheck of name:string option
    | PortCheckLoaded of Result<PingStatus,exn>
    | PortInitMerge of Port list
    | PortInitFail of exn

type Model = {PortName:string; PortInput:int option; Ports:Map<int,string option*PingStatus option>;Error:string option}

module PorterImpl =
    open Fable.PowerPack.Fetch
    open Microsoft.FSharp.Core
    let listenerCheck port =
        fetchAs<PingStatus> (sprintf "/api/ping?port=%i" port) (Decode.Auto.generateDecoder())
    let watchKey = "Porter.PorterImpl.watchKey"
    let initCmd:Cmd<Msg> =
        Cmd.ofFunc (fun () ->
            printfn "Porter.initCmd starting"
            match Storage.get watchKey with
            | None
            | Some [] -> printfn "Found key";[]
            | Some x ->
                printfn "Found key"
                x
            )
            ()
            Msg.PortInitMerge
            Msg.PortInitFail


    //consider loading up from localstorage any saved ports and listening for them
    let init () : Model * Cmd<Msg> =
        {PortName="";PortInput = None;Ports=Map.empty;Error = None},initCmd


    let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
        match msg with
        | PortInitFail x ->
            printfn "Merging ports failed"
            {currentModel with Error=Some x.Message },Cmd.none
        | PortInitMerge x ->
            printfn "Merging ports"
            currentModel, Cmd.none
        | PortNamed raw -> {currentModel with PortName = raw},Cmd.none
        | PortChanged raw ->
            match System.Int32.TryParse raw with
            | true, x ->
                {currentModel with PortInput = Some x;Error=None}, Cmd.none
            | false, _ -> {currentModel with Error= Some "Failed to read port";PortInput=None}, Cmd.none
        | PortCheck name ->
            match currentModel.PortInput with
            | None ->
                {currentModel with Error=Some "No valid port to check"}, Cmd.none
            | Some port ->
                let cmd = Cmd.ofPromise (listenerCheck port) [] (Ok >> PortCheckLoaded) (Microsoft.FSharp.Core.Error >> PortCheckLoaded)
                let ps = currentModel.Ports |> Map.add port (name |> Option.bind Option.ofValueString,None)
                Browser.console.log(name,port)
                {currentModel with Ports = ps; Error = None}, cmd
        | PortCheckLoaded (Ok ps) ->
            printfn "Port Check loaded"
            currentModel.Ports
            |> Map.tryFind ps.Port
            |> function
                | Some (name,_) ->
                    let ps =
                        currentModel.Ports
                        |> Map.add ps.Port (name,Some ps)
                    {currentModel with Ports = ps}, Cmd.none
                | None -> {currentModel with Error = Some <| sprintf "PortCheck status returned for unloaded port:%i" ps.Port},Cmd.none
        | PortCheckLoaded (Error ex) ->
            {currentModel with Error = Some <| sprintf "%A" ex}, Cmd.none
module Run =
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    let view (model : Model) (dispatch : Msg -> unit) =
      let portCheckMsg = PortCheck <| Option.ofValueString model.PortName

      Container.container [][
        Label.label [] [str "Enter port to check"]
        Input.text [Input.Placeholder "Ex:postgres"
                    Input.Value model.PortName
                    Input.Color (if model.Error.IsSome then Color.IsDanger else Color.IsSuccess)
                    Input.Props [ OnChange (fun ev -> dispatch (PortNamed !!ev.target?value)) ]
        ]
        Input.number [Input.Placeholder "Ex: 5432"
                      Input.Value (string model.PortInput)
                    //   Input.Modifiers [ Modifier.TextTransform TextTransform.UpperCase ]
                      Input.Color (if model.Error.IsSome then Color.IsDanger else Color.IsSuccess)
                      Input.Props [ OnChange (fun ev -> dispatch (PortChanged !!ev.target?value))
                                    onKeyDown KeyCode.enter (fun _ -> dispatch portCheckMsg) ] ]
        Label.label [Label.CustomClass "error"] [ model.Error |> Option.defaultValue null |> str ]
        Container.container [] [
            table [ClassName "table table-striped"] [
                thead [] [
                    tr [] [
                        th [] [str ""]
                        th [] [str "Name"]
                        th [] [str "Port"]
                        th [] [str "Open"]
                        th [] [str "Updated"]
                        th [] [str ""]
                    ]
                ]
                tbody [] [
                    yield!
                        model.Ports
                        |> Map.toSeq
                        |> Seq.map(fun (port,(name,ps))->
                            tr[] [
                                yield td [] [str ""]
                                match name with
                                | Some (ValueString name) ->
                                    yield td[] [str name]
                                | _ -> yield td[] []
                                yield td [] [ !!port]
                                match ps with
                                | Some ps ->
                                    yield td [ if ps.IsOpen then yield ClassName "is-success" else yield ClassName "is-danger"] [str <| string ps.IsOpen]
                                    yield td [ClassName "error"] [str <| string ps.Error]
                                | None -> ()
                            ]
                        )
                ]

            ]
            ul [] [
            ]
        ]

        // Label.label [] [ model.PingStatus |> Option.map (fun ps -> sprintf "Port %i - %s" ps.Port ps.PortStatus) |> Option.defaultValue "" |> str]
      ]


