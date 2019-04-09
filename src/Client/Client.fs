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
type ClientMode =
    |Loading of string
    |CounterMode
    |PortMode
    |P5Mode

type Loader =
    |PortLoader
    |P5Loader
// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    |CounterMsg of CounterApp.Msg
    |PorterMsg of Porter.Msg
    |InitializePorter
    |P5Msg of P5Routing.Msg
    |ChangeMode of ClientMode

type Model = { ClientMode:ClientMode; CounterModel: CounterApp.Model ; PorterModel:Porter.Model; P5Model: P5Routing.Model;LoadMap:Map<Loader,Cmd<Msg>>}


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let p5Model,pCmd = P5Routing.P5Impl.init()
    let cModel,cCmd = CounterApp.init()
    let portModel,prtCmd = Porter.PorterImpl.init()
    let initialModel =
        let loadMap = Map[
            Loader.PortLoader,prtCmd |> Cmd.map Msg.PorterMsg
            Loader.P5Loader, pCmd |> Cmd.map Msg.P5Msg
        ]
        {ClientMode=CounterMode; CounterModel = cModel ; PorterModel = portModel; P5Model=p5Model;LoadMap=loadMap}
    // initialModel, Cmd.batch [(loadCountCmd |> Cmd.map CounterMsg); cmd |> Cmd.map P5Msg]
    initialModel, cCmd |> Cmd.map Msg.CounterMsg



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let onIfP5ModeLeaving nextModel=
        match nextModel with
        | {ClientMode=P5Mode} -> ()
        | _ ->
            match currentModel with
            | {ClientMode=P5Mode} ->
                P5Routing.P5Impl.cleanUp()
            | _ -> ()
        nextModel
    let nextModel,cmd =
        match msg with
        | ChangeMode mode ->
            {currentModel with ClientMode=mode}, Cmd.none
        | InitializePorter ->
            let m,cmd = Porter.PorterImpl.init()
            {currentModel with PorterModel = m; ClientMode=ClientMode.PortMode}, cmd |> Cmd.map Msg.PorterMsg
        | Msg.PorterMsg msg ->
            // ugh optional model is a problem
            let model, cmd = Porter.PorterImpl.update msg currentModel.PorterModel
            {currentModel with PorterModel=model}, cmd |> Cmd.map Msg.PorterMsg
        | P5Msg msg ->
            let model,cmd = P5Routing.P5Impl.update msg currentModel.P5Model
            {currentModel with P5Model = model}, cmd |> Cmd.map Msg.P5Msg
        | CounterMsg msg ->
            let model,cmd = CounterApp.update msg currentModel.CounterModel
            {currentModel with CounterModel = model}, cmd |> Cmd.map Msg.CounterMsg
    let nextModel =
        (nextModel,[ onIfP5ModeLeaving])
        ||> List.fold(fun nextModel f ->
            f nextModel
        )
    nextModel, cmd


open Elmish
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
        [ strong [] [ str "SAFE Template" ]
          str " powered by: "
          components ]

let loadingView text =
    Container.container [] [
        str text
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [] [ str "SAFE Template" ] ]
              Navbar.Item.div [][
                  div [] [ pButton "Home" (fun _ -> dispatch <| ChangeMode ClientMode.CounterMode)]
                  div [] [ pButton "Porter" (fun _ -> dispatch <| ChangeMode ClientMode.PortMode)]
                  div [] [ pButton "P5" (fun _ -> dispatch <| ChangeMode ClientMode.P5Mode )]

              ]
            ]
          (match model.ClientMode with
            | CounterMode -> CounterApp.view model.CounterModel (Msg.CounterMsg>>dispatch)
            | Loading text -> loadingView text
            | PortMode -> Porter.Run.view model.PorterModel (Msg.PorterMsg>>dispatch)
            | P5Mode ->
                printfn "Running in P5 mode"
                P5Routing.Run.view model.P5Model (P5Msg>>dispatch)
          )
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
