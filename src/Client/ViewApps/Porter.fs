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
open ClientShared.Controls


type Name = string
type Ident = int
type Port = {Name:Name option;Ident:Ident}
type PortMap = Map<int,string option*PingStatus option>

type Msg =
    | PortNamed of string
    | PortChanged of string
    | PortCheck of name:string option
    | PortCheckLoaded of Result<PingStatus,Ident*exn>
    | PortInitMerge of Port list
    | PortInitFail of exn
    | SetPoll of bool
[<Measure>] type s
type Model = {PortName:string; PortInput:int option; Ports:PortMap;Error:string option;Sleep:int<s>;Poll:bool}

module PorterImpl =
    open Fable.PowerPack.Fetch
    open Fable.PowerPack
    open Microsoft.FSharp.Core
    let listenerCheck sleep port:JS.Promise<PingStatus> =
        printfn "making a promise"
        promise {
            if sleep > 0<s> then
                printfn "Sleeper sleeping for ident:%i (%i second(s))" port sleep
                do! Promise.sleep (int sleep * 1000)
            printfn "Fetching!"
            let! result = fetchAs<PingStatus> (sprintf "/api/ping?port=%i" port) (Decode.Auto.generateDecoder<PingStatus>(isCamelCase=false)) []
            // match Thoth.Json.Decode.Auto.fromString<PingStatus>(result) with
            // | Ok x -> return x
            // | Error r -> return invalidOp <| sprintf "Deserialization failed %s" r
            return result
        }
    let storage = Storage.store<Port list>("Porter.PorterImpl.watchKey")
    let addPort port name ps (m:PortMap) = m |> Map.add port (name |> Option.bind Option.ofValueString,ps)
    let inline foldPort m port (name,ps) = addPort port name ps m
    let inline insertPorts m port =
        foldPort m port.Ident (port.Name,None)


    let initCmd:Cmd<Msg> =
        Cmd.ofFunc (fun () ->
            printfn "Porter.initCmd starting"
            try
                match storage.Get() with
                | None -> printfn "No key found";None
                | Some [] -> printfn "Found empty key";None
                | Some x ->
                    printfn "Found key"
                    Some x
            with ex ->
                eprintfn "Error checking storage:%s" ex.Message
                None
            |> Option.defaultValue List.empty
            )
            ()
            Msg.PortInitMerge
            Msg.PortInitFail


    //consider loading up from localstorage any saved ports and listening for them
    let init () : Model * Cmd<Msg> =
        {PortName="";PortInput = None;Ports=Map.empty;Error = None;Sleep=5<s>;Poll=false},initCmd


    let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
        printfn "update fired! %A" msg
        let listenerCmd sleep port = Cmd.ofPromise (listenerCheck sleep) port (Ok >> PortCheckLoaded) (fun e -> PortCheckLoaded(Error(port,e)))
        let listenCurrent forcePing model =
            let batchListen ports = Cmd.batch (ports |> Seq.map (listenerCmd model.Sleep))
            let shouldPing = model.Poll || forcePing
            printfn "ListenCurrent will ping? %b" shouldPing
            if shouldPing then
                model.Ports
                |> Map.toSeq
                |> Seq.map fst
                |> batchListen
            else Cmd.none

        match msg with
        | SetPoll(poll) ->
            match currentModel.Poll, poll with
            | false,false
            | true, _ ->
                {currentModel with Poll=false},Cmd.none
            | false, true ->
                let nextModel = {currentModel with Poll=true}
                nextModel, listenCurrent false nextModel
        | PortInitFail x ->
            printfn "Merging ports failed"
            {currentModel with Error=Some x.Message },Cmd.none
        | PortInitMerge x ->
            printfn "Merging ports"
            let ps =
                (currentModel.Ports,x)
                ||> Seq.fold insertPorts
            let nextModel ={currentModel with Ports=ps}
            let cmd = listenCurrent true nextModel
            nextModel, cmd
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
                let ps = currentModel.Ports |> addPort port (name |> Option.bind Option.ofValueString) None
                Browser.console.log(name,port)
                {currentModel with Ports = ps; Error = None}, listenerCmd 0<s> port
        | PortCheckLoaded (Ok ps) ->
                printfn "Port Check loaded"
                currentModel.Ports
                |> Map.tryFind ps.Port
                |> function
                    | Some (name,_) ->
                        let prts = addPort ps.Port name (Some ps) currentModel.Ports
                        prts
                        |> Map.toSeq
                        |> Seq.map (fun (k,(v,_)) -> {Ident=k;Name=v})
                        |> List.ofSeq
                        |> storage.Set
                        {currentModel with Ports = prts}, if currentModel.Poll then listenerCmd currentModel.Sleep ps.Port else Cmd.none
                    | None -> {currentModel with Error = Some <| sprintf "PortCheck status returned for unloaded port:%i" ps.Port},Cmd.none
        | PortCheckLoaded (Error (p,ex)) ->
            {currentModel with Error = Some <| sprintf "%A" ex}, if currentModel.Poll then  listenerCmd currentModel.Sleep p else Cmd.none
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
        pButton (if model.Poll then "Stop" else "Start") (fun _ -> dispatch (SetPoll <| not model.Poll))
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
                                    yield td [] [ str <| ps.When.ToLongTimeString() ]
                                    yield td [ClassName "error"] [str <| string ps.ErrorMsg]
                                | None ->
                                    yield td [] []
                                    yield td [] []
                                    yield td [] []

                            ]
                        )
                ]

            ]
            ul [] [
            ]
        ]

        // Label.label [] [ model.PingStatus |> Option.map (fun ps -> sprintf "Port %i - %s" ps.Port ps.PortStatus) |> Option.defaultValue "" |> str]
      ]


