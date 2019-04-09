module Client


open Elmish
open Fable.Core.JsInterop
open Fable.PowerPack.Fetch
open Thoth.Json

open Shared
open ClientShared
open ClientShared.Controls



// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
[<RequireQualifiedAccess>]
type ClientApp=
    |Counter
    |Porter
    |P5

type Loader =
    |PortLoader
    |P5Loader
// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
[<RequireQualifiedAccess>]
type AppMsg =
    |Counter of CounterApp.Msg
    |Porter of Porter.Msg
    |P5 of P5Routing.Msg
type Msg =
    |AppMsg of AppMsg
    |ChangeMode of ClientApp

type Model = { ClientMode:ClientApp; CounterModel: CounterApp.Model ; PorterModel:Porter.Model; P5Model: P5Routing.Model;LoadMap:Map<ClientApp,Cmd<Msg>>}


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let cmdMap am = Cmd.map (am >> Msg.AppMsg)
    let p5Model,pCmd = P5Routing.P5Impl.init()
    let portModel,prtCmd = Porter.PorterImpl.init()
    let cModel,cmd = CounterApp.init()
    let initialModel =
        let loadMap = Map[
            ClientApp.Porter,prtCmd |> cmdMap AppMsg.Porter
            ClientApp.P5, pCmd |> cmdMap AppMsg.P5
        ]
        {ClientMode=ClientApp.Counter; CounterModel = cModel ; PorterModel = portModel; P5Model=p5Model;LoadMap=loadMap}
    // initialModel, Cmd.batch [(loadCountCmd |> Cmd.map CounterMsg); cmd |> Cmd.map P5Msg]
    initialModel, cmd |> cmdMap AppMsg.Counter



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let onIfP5ModeLeaving nextModel=
        match nextModel with
        | {ClientMode=ClientApp.P5} -> ()
        | _ ->
            match currentModel with
            | {ClientMode=ClientApp.P5} ->
                P5Routing.P5Impl.cleanUp()
            | _ -> ()
        nextModel
    let nextModel,cmd =
        match msg with
        | ChangeMode mode ->
            currentModel.LoadMap
            |> Map.tryFind mode
            |> function
                | Some cmd ->
                    {currentModel with ClientMode=mode},cmd
                | None -> {currentModel with ClientMode=mode}, Cmd.none
        | Msg.AppMsg msg ->
            let cmdMap am = Cmd.map (am >> Msg.AppMsg)
            match msg with
            | AppMsg.Porter msg ->
                // ugh optional model is a problem
                let model, cmd = Porter.PorterImpl.update msg currentModel.PorterModel
                {currentModel with PorterModel=model}, cmd |> cmdMap AppMsg.Porter
            | AppMsg.P5 msg ->
                let model,cmd = P5Routing.P5Impl.update msg currentModel.P5Model
                {currentModel with P5Model = model}, cmd |> cmdMap AppMsg.P5
            | AppMsg.Counter msg ->
                let model,cmd = CounterApp.update msg currentModel.CounterModel
                {currentModel with CounterModel = model}, cmd |> cmdMap AppMsg.Counter
    let nextModel =
        (nextModel,[ onIfP5ModeLeaving])
        ||> List.fold(fun nextModel f ->
            f nextModel
        )
    nextModel, cmd

open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma

let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://github.com/giraffe-fsharp/Giraffe" ] [ str "Giraffe" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
           ]

    p [ ]
        [ strong [] [ str "my Giraffe" ]
          str " powered by: "
          components ]

let loadingView text =
    Container.container [] [
        str text
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    let dispatchApp am = (am >> Msg.AppMsg >> dispatch)
    let appView =
          match model.ClientMode with
          | ClientApp.Counter -> CounterApp.view model.CounterModel (dispatchApp AppMsg.Counter)
          | ClientApp.Porter -> Porter.Run.view model.PorterModel (dispatchApp AppMsg.Porter)
          | ClientApp.P5 -> P5Routing.Run.view model.P5Model (dispatchApp AppMsg.P5)

    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ] [ Heading.h2 [] [ str "my giraffe" ] ]
              Navbar.Item.div [][
                  div [] [ pButton "Counter" (fun _ -> dispatch <| ChangeMode ClientApp.Counter)]
                  div [] [ pButton "Porter" (fun _ -> dispatch <| ChangeMode ClientApp.Porter)]
                  div [] [ pButton "P5" (fun _ -> dispatch <| ChangeMode ClientApp.P5)]
              ]
            ]
          appView
          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
